#Read in list of all Authors 

library(tidyverse)


all_authors <- read.csv("Authorship on Forecast Evaluation Manuscript.csv") %>%
  select(Team.Name, first_name =  "Author.First.Name.s....Initial", last_name = "Author.Last.Name") %>%
  mutate(Name = paste(first_name, last_name)) %>%
  filter(!Name %in% c("Estee Y Cramer", "Evan L Ray", "Johannes Bracher", "Evan Ray",
                     "Matthew Biggerstaff","Michael Johansson", "Velma K Lopez", "Jo W Walker", 
                     "Rachel B Slayton", "Rachel B Slayton")) %>%
  mutate(Last_first  = paste(last_name, first_name)) 
  

team_leads <- read.csv("All_Leads.csv") %>%
  mutate(Last_first = paste(Last, First))


order_names <- all_authors %>%
  full_join(team_leads, by = "Team.Name") %>%
  mutate(team_lead = ifelse(Last_first.x %in% team_leads$Last_first, "1", "0")) %>%
  mutate(Name = ifelse(is.na(Name.x), as.character(Name.y), Name.x),
         Last_first = ifelse(is.na(Last_first.x), as.character(Last_first.y), Last_first.x)) %>%
  filter(!(Team.Name %in% c("Wadhwani_AI-BayesOpt","JHU_CSSE-DECOM","JCB-PRM", "epiforecasts-ensemble1",
                            "CAN-SEIR_CAN"))) %>%
  filter(!(Name %in% c("Evan Ray", "Timothy L. Snyder", "Sansiddh Jain"))) %>%
  distinct(Name, .keep_all = TRUE) %>%
  group_by(Team.Name) %>%
  ungroup() %>%
  arrange(Order, desc(team_lead), last_name) 


paste0(order_names$Name, collapse = ", ")


 

all_authors_test <- read.csv("Authorship on Forecast Evaluation Manuscript.csv") %>%
  select(Team.Name, first_name =  "Author.First.Name.s....Initial", last_name = "Author.Last.Name") %>%
  mutate(Name = paste(first_name, last_name)) %>%
  group_by(Name) %>%
  summarise(n = n()) %>%
  filter(n > 1)



count_people <- c("Estee Y Cramer, Evan L Ray, Johannes Bracher, Aaron Gerding, Alvaro J Castro Rivadeneira, Apurv Shah, Ariane Stark, Ayush Khandelwal, Dasuni Jayawardena , Kanji Abdul H, Katie H House, Khoa Le, Martha W Zorn, Nicholas G Reich, Nutcha Wattanachit, Tilmann Gneiting, Yijin Wang, Yuxin Huang, Youyang Gu, Spencer Woody, James G Scott, Kelly Gaither, Lauren A Meyers, Mauricio Tec, Maytal Dahan, Michael Lachmann, Spencer Fox, Benjamin D Trump, Brandon Lafferty, Glover E George, Ian D Dettwiller, Igor Linkov, Jeffrey C Cegan, Matthew D Parno, Matthew W Farthing, Michael A Rowland, Michael L Mayo, Robert H Hunter, William P England, Ella McCauley, Emily T Martin, Karl Falb, Marisa C Eisenberg, Robert L Myers, Sabrina M Corsetti, Thomas M Baer, Tom Schwarz, Yitao Huang, Daniel Sheldon, Graham Casey Gibson, Quanquan Gu, Difan Zou, Jinghui Chen, Lingxiao Wang, Pan Xu, Weitong Zhang, Hannah Biegel, Joceline Lega, Robert Walraven, James A Turtle, Michal Ben-Nun, Pete Riley, Steven Riley, Ugur Koyluoglu, Alexander Wong, Barrie Wilkinson, Bruce Hamory, Chris Schrader, Chris Stiefeling, Christina Kyriakides, Daniel Siegel, David DesRoches, Elizabeth Shakhnovich, Gokce Ozcan, Helen Leis, James Morgan, John Milliken, Michael Moloney, Ryan Spatz, Sean Cavany, Alex Perkins, Guido España, Rachel Oidtman, Sean Moore, Alessandro Vespignani, Ana Pastore y Piontti, Jessica T Davis, Kunpeng Mu ,Matteo Chinazzi, Xinyue Xiong, Dave Osthus, Geoffrey Fairchild, Isaac Michaud, Lauren Castro, Dean Karlen, Claire P Smith, Elizabeth C Lee, Hannah R Meredith, Javier Perez-Saez, Joseph C Lemaitre, Josh Wills, Joshua Kaminsky, Juan Dent, Justin Lessler, Kathryn Kaminsky, Kyra H Grantz, Lindsay T Keegan, Sam Shah, Shaun A Truelove, Stephen A Lauer, Guannan Wang, Lei Gao, Lily Wang, Myungjin Kim, Shan Yu, Xinyi Li, Yueying Wang, Zhiling Gu, Chris Murray, David Pigott, Emmanuela Gaikedu, Robert C Reiner, Ryan Barber, Simon Hay, Steve Lim, B. Aditya Prakash, Alexander Rodríguez, Anika Tabassum, Bijaya Adhikari, Jiajia Xie, Jiaming Cui, Jeffrey Shaman, Sen Pei, Teresa K Yamana, Evan Ray, Benjamin P Linas, Jade Xiao, Jagpreet Chhatwal, Madeline Adee, Mary A Ladd, Ozden O Dalgic, Peter Mueller, Turgay Ayer, Alden Green, Aaron Rumack, Addison J Hu, Balasubramanian Narasimhan, Jacob Bien, Larry Wasserman, Maria Jahja, Noah Simon, Rob Tibshirani, Ryan Tibshirani, Samyak Rajanala, Valerie Ventura, Ajitesh Srivastava, Dongxia Wu, Liyao Gao, Rose Yu, Yian Ma, Xiaoyong Jin, Xifeng Yan, Yu-Xiang Wang, Lihong Guo, YangQuan Chen, Yanting Zhao, Timothy L. Snyder, Davison D Wilson, Steve McConnell, Xuegang Ban, Yunfeng Shi, Qi-Jun Hong, Stanley Kong, Chaozhuo Li, Jiang Bian, Juan Lavista Ferres, Shun Zhang, Shun Zheng, Tie-Yan Liu, Wei Cao, Xing Xie, Zhifeng Gao, Andreea Georgescu, Andrew Zheng, Deeksha Sinha, Jackie Baek, Joshua Wilde, Retsef Levi, Vivek Farias, Leo A Celi, Nicolas D Penna, Saketh Sundar, Matt Kinsey, Katharine Tallaksen, RF Obrecht, Arden Baxter, Buse Eylul Oruc, John Asplund, Nicoleta Serban, Pinar Keskinocak, Arkady Epshteyn, Chun-Liang Li, Dario Sava, Elli Kanal, Jinsung Yoon, Leyou Zhang, Long T Le, Mike Dusenberry, Nate Yoder, Rajarishi Sinha, Sercan O Arik, Thomas Tsai, Tomas Pfister, Sam Abbott, Mingyuan Zhou, Rahi Kalantari, Dimitris Bertsimas, Hamza Tazi Bouardi, Michael L Li, Omar Skali Lami, Saksham Soni, Donglin Zeng, Qinxia Wang, Shanghong Xie, Yuanjia Wang, Eamon B O'Dea, John M Drake,
                  Robert Pagano, Jo W Walker, Velma K Lopez, Rachel B Slayton, Michael Johansson, Matthew Biggerstaff, Nicholas G Reich")