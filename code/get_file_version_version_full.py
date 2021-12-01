# This script queries the time stamps of when forecasts and their different versions
# are merged into covid19-forecast-hub repo, and decides which version has been upload
# to Zoltar. The following process describes the logic of this script
# 
# 0. Set up GitHub personal access token: https://docs.github.com/en/free-pro-team@latest/github/authenticating-to
# -github/creating-a-personal-access-token and put the token inside an environment variable "auth_token" 1. Gets a
# list of model directories under the data-processed/ directory in the covid19-hub-forecast repo 2. For each of these
# model directories, traverse through the forecast files 3. For each forecast file, 3.1 skip if they are pre-May
# forecasts, since they are mostly merged directly into the repo, not through Pull Request, thus harder to keeping
# track of time stamp automatically 3.2 Queries from GitHub the list of times when this file were merged into the
# repo through Pull Requests, and the files that were associated with these times 3.3 Compare the files that were
# associated with those times to the file uploaded to Zoltar to determine which version is pushed to Zoltar 3.4 Write
# the versions' timestamps got from 3.2, and the version that was uploaded to Zoltar determined by 3.3 into a csv
# file 4. There will be discrepancies between the output of this file and the correct file that was sent. There are a
# couple of reasons. one is because of a lot of early forecasts are merged directly into master and involves
# moving/deleting rather than changing its content. Another reason is that Zoltar seems to round some numbers,
# which made the version on Zoltar different from the one on GitHub on some rows An example of this is
# 2020-09-13-JHU_IDD-CovidSP.csv
#
#
# 2021-06-21: Updated by Matt Cornell (cornell@umass.edu) to handle GitHub API rate limits that this program is now
# hitting ("API rate limit exceeded for user ID"), presumably due to Zoltar having many more forecasts than the last
# time the program was run. See `get_repo()`'s "sleep if hitting rate limit" section.
#
# To run the program:
# - get a GitHub personal access token as above
# $ cd <this repo's root dir>
# $ mkdir individual_models/
# $ pipenv --three
# $ pipenv install
# $ pipenv shell
# $ export auth_token=<token>
# $ export Z_USERNAME=<your zoltar username>
# $ export Z_PASSWORD=<your zoltar password>
# $ python3 code/get_file_version_version_full.py <path_to_covid19-forecast-hub/data-processed>
# Note that it took ~seven hours to complete.


import calendar
import io
import os
import time
from datetime import timezone

import click
import pandas as pd
from github import Github
from zoltpy import util
from zoltpy.connection import ZoltarConnection
from zoltpy.covid19 import COVID_TARGETS, covid19_row_validator, COVID_ADDL_REQ_COLS
from zoltpy.quantile_io import json_io_dict_from_quantile_csv_file


@click.command()
@click.argument('data_processed_dir', type=click.Path(file_okay=False, exists=True))
def app(data_processed_dir):
    # Get all models' directory
    path_to_data = data_processed_dir
    list_of_model_directories = []
    for directory in os.listdir(path_to_data):
        if "." not in directory:
            list_of_model_directories.append(directory)
    df = pd.DataFrame(columns={"model": str,
                               "timezero": str,
                               "v1_github_timestamp": str,
                               "v2_github_timestamp": str,
                               "v3_github_timestamp": str,
                               "in_zoltar": str})

    # Connect to Zoltar
    conn = ZoltarConnection()
    conn.authenticate(os.environ.get("Z_USERNAME"), os.environ.get("Z_PASSWORD"))

    # Loop through each model and perform the query
    for model in list_of_model_directories:
        forecasts = os.listdir(path_to_data + model)
        model_df = pd.DataFrame(columns={"model": str,
                                         "timezero": str,
                                         "v1_github_timestamp": str,
                                         "v2_github_timestamp": str,
                                         "v3_github_timestamp": str,
                                         "in_zoltar": str})
        try:
            for forecast in forecasts:
                forecast_df = pd.DataFrame(columns={"model": str,
                                                    "timezero": str,
                                                    "v1_github_timestamp": str,
                                                    "v2_github_timestamp": str,
                                                    "v3_github_timestamp": str,
                                                    "in_zoltar": str})
                # Skip metadata text file
                if not forecast.endswith('.csv'):
                    continue
                time_zero_date = forecast.split(model)[0][:-1]
                month = time_zero_date.split('-')[1]
                year = time_zero_date.split('-')[0]
                if int(month) < 5 and year == '2020':
                    continue
                forecast_df.at[0, 'timezero'] = time_zero_date
                model_name = model.split('/')[-1]
                forecast_df.at[0, 'model'] = model_name
                result = None
                time_stamps, forecast_files = get_github_time_stamps_of_forecast(model_name, time_zero_date)

                if len(time_stamps) != len(forecast_files):
                    print(forecast)
                    print("sth wrong")
                    break
                forecast_df.at[0, 'v1_github_timestamp'] = time_stamps[0]
                if len(time_stamps) < 2:
                    forecast_df.at[0, 'in_zoltar'] = 'v1'
                else:
                    result = compare_forecast(conn, model_name, time_zero_date, forecast_files)
                    print(result)
                    if len(time_stamps) >= 2:
                        forecast_df.at[0, 'v2_github_timestamp'] = time_stamps[1]
                    if len(time_stamps) >= 3:
                        forecast_df.at[0, 'v3_github_timestamp'] = time_stamps[2]
                    if True not in result:
                        print("Forecast version does not match on zoltar for this forecast: " + forecast)
                        continue
                    for res in range(len(result)):
                        if result[res]:
                            forecast_df.at[0, 'in_zoltar'] = 'v' + str(res + 1)
                model_df = model_df.append(forecast_df)
                print(forecast)
                time.sleep(0.25)
        except Exception as ex:
            print(ex)
            continue
        df = df.append(model_df)
        model_df.to_csv("individual_models/" + model + ".csv", index=False)

    # finish
    list_of_results = os.listdir("individual_models")
    for result in list_of_results:
        model_df = pd.read_csv('individual_models/' + result)
        df = df.append(model_df)
    df = df.sort_values(['model', 'timezero'])
    df.to_csv("model-forecast-versions-latest.csv", index=False)


# Change the time queried to EST time zone
def utc_to_local(utc_dt):
    return utc_dt.replace(tzinfo=timezone.utc).astimezone(tz=None)


# Get the repository object through github API
def get_repo():
    g = Github(os.environ.get("auth_token"))

    # sleep if hitting rate limit. https://github.com/PyGithub/PyGithub/issues/1319
    rate_limit = g.get_rate_limit()
    core_rate_limit = rate_limit.core
    remaining = core_rate_limit.remaining
    if remaining < 20:  # ~5 API calls between each call to this function
        reset_timestamp = calendar.timegm(core_rate_limit.reset.timetuple())
        # add seconds to be sure the rate limit has been reset:
        sleep_time = reset_timestamp - calendar.timegm(time.gmtime()) + 60  # second. was 5
        print(f"limit low. rate_limit={rate_limit}, reset_timestamp={reset_timestamp}, sleep_time={sleep_time}")
        if sleep_time > 0:  # sometimes it's negative. Q: why?
            time.sleep(sleep_time)
        else:
            time.sleep(60)  # hack

        rate_limit = g.get_rate_limit()
        print(f"awake. rate_limit={rate_limit}")
    else:
        print(f"limit ok. rate_limit={rate_limit}")

    # continue
    covid_repo = g.get_repo("reichlab/covid19-forecast-hub")
    return covid_repo


# A function that takes a model name and a timezero as input
# to identify the corresponding forecasts and queries the timestamps
# of different version of this forecast    
#
# Return: a list of timestamps of each version and a list of files associated with each version
def get_github_time_stamps_of_forecast(model, timezero):
    covid_repo = get_repo()
    filepath = "data-processed/" + model + "/" + timezero + "-" + model + ".csv"
    commits = covid_repo.get_commits(path=filepath)
    pull_requests = set()
    time_stamps = []
    forecast_files = []
    for commit in commits:
        if commit.get_pulls().totalCount > 0:
            pr = commit.get_pulls()[0]
            if pr.number == 720:
                continue
            pr_id = pr.id
            if pr_id in pull_requests:
                continue
            files = pr.get_files()
            for forecast_file in files:
                if forecast_file.filename == filepath:
                    forecast_files.append(forecast_file)
                    break
            pull_requests.add(pr_id)
            time_stamps.append(utc_to_local(pr.merged_at).strftime("%m/%d/%y-%H:%M:%S"))
        else:
            time_stamps.append("None")
            files = commit.files
            for forecast_file in files:
                if forecast_file.filename == filepath:
                    forecast_files.append(forecast_file)

    time_stamps.reverse()
    forecast_files.reverse()
    return time_stamps, forecast_files


def my_floor(a, precision=0):
    return ((a * 10 ** precision) // 1) / (10 ** precision)


# A function that takes a model name, a timezero and a list of files
# as input. It will use the model name and time zero to query the corresponding
# forecast from Zoltar, and compare its content to the list of files provided
# and determine which files in the list of file match the forecast on Zoltar,
# i.e which version
#
# Return: a list of booleans that indicate which of the input files in 
#     github_forecasts matched with the one uploaded to Zoltar
def compare_forecast(conn, model, timezero, github_forecasts):
    data = util.download_forecast(conn, 'COVID-19 Forecasts', model, timezero)
    zoltar_df = util.dataframe_from_json_io_dict(data)
    zoltar_df['unit'] = zoltar_df['unit'].apply(lambda x: '{0:0>5}'.format(x))
    zoltar_df = zoltar_df.sort_values(['unit', 'target', 'class', 'quantile'])
    zoltar_df = zoltar_df.reset_index(drop=True)
    zoltar_df = zoltar_df.astype({'value': float})
    zoltar_df['value'] = zoltar_df['value'].apply(lambda x: my_floor(x, 0))
    result = []
    forecast_df = None
    for forecast in github_forecasts:
        raw_forecast_df = pd.read_csv(forecast.raw_url)
        if model in ['UT-Mobility', 'LANL-GrowthRate', 'FAIR-NRAR']:
            raw_forecast_df = raw_forecast_df.astype({'location': str})
            raw_forecast_df['location'] = raw_forecast_df['location'].apply(lambda x: '{0:0>5}'.format(x))
        s_buf = io.StringIO()
        raw_forecast_df.to_csv(s_buf)
        s_buf.seek(0)
        quantile_json, error_from_transformation = json_io_dict_from_quantile_csv_file(s_buf,
                                                                                       COVID_TARGETS,
                                                                                       covid19_row_validator,
                                                                                       COVID_ADDL_REQ_COLS)
        forecast_df = util.dataframe_from_json_io_dict(quantile_json)
        forecast_df['unit'] = forecast_df['unit'].apply(lambda x: '{0:0>5}'.format(x))
        forecast_df = forecast_df.sort_values(['unit', 'target', 'class', 'quantile'])
        forecast_df = forecast_df.reset_index(drop=True)
        forecast_df = forecast_df.astype({'value': float})
        forecast_df['value'] = forecast_df['value'].apply(lambda x: my_floor(x, 0))
        result.append(zoltar_df.equals(forecast_df))
    return result


if __name__ == '__main__':
    app()
