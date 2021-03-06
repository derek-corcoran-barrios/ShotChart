shinyUI(fluidPage(
  
  titlePanel("ShotCharts"),
  tabsetPanel(tabPanel("Team shot charts", sidebarLayout(
    sidebarPanel( h3("Offensive shot chart"),
                  p("In this shot chart you can see either a Points per shot or percentage representation of a shot chart. The bigger the hexagon, the more common the shot is. The Points per shot scale or percentage ar shown in a color scheme"),
                  selectInput(inputId = "OffTeam",
                              label = "Select Team:",
                              choices =  c("Atl", "Bkn", "Bos", "Cha", "Chi", "Cle", "Dal", "Den", "Det", 
                                           "GSW", "Hou", "Ind", "Lac", "Lal", "Mem", "Mia", "Mil", "Min", 
                                           "NO", "NY", "Okc", "ORL", "Phi", "Pho", "Por", "Sac", "Sas", 
                                           "Tor", "Uta", "Was"),
                              selected = "GSW"
                  ),
                  selectInput(inputId = "OffType",
                              label = "Points per shot or Percentage:",
                              choices = c("PPS", "PCT"),
                              selected = "PPS"
                  ),
                  h3("Defensive shot chart"),
                  p("In this shot chart you can see either a Points per shot or percentage representation of a shot chart. The bigger the hexagon, the more common the shot is. The Points per shot scale or percentage ar shown in a color scheme"),
                  selectInput(inputId = "DefTeam",
                              label = "Select Team:",
                              choices =  c("Atl", "Bkn", "Bos", "Cha", "Chi", "Cle", "Dal", "Den", "Det", 
                                           "GSW", "Hou", "Ind", "Lac", "Lal", "Mem", "Mia", "Mil", "Min", 
                                           "NO", "NY", "Okc", "ORL", "Phi", "Pho", "Por", "Sac", "Sas", 
                                           "Tor", "Uta", "Was"),
                              selected = "Mem"
                  ),
                  selectInput(inputId = "DefType",
                              label = "Points per shot or Percentage:",
                              choices = c("PPS", "PCT"),
                              selected = "PPS"
                  )),
    mainPanel(plotOutput("distPlot"),
              downloadLink("downloadOff", "Download"),
              plotOutput("defPlot"),
              downloadLink("downloadDef", "Download"),
              p("Data updated on", textOutput("Updated")))
  )),
              tabPanel("Player shot charts", sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "player",
                              label = "Select Player:",
                              choices =  c("Aaron Brooks", "Aaron Gordon", "Abdel Nader", 
                                           "Alec Burks", "Alec Peters", "Alex Abrines", "Alex Caruso", "Alex Len", 
                                           "Alex Poythress", "Al-Farouq Aminu", "Alfonzo McKinnie", "Al Horford", 
                                           "Al Jefferson", "Allen Crabbe", "Amir Johnson", "Andre Drummond", 
                                           "Andre Iguodala", "Andre Roberson", "Andrew Bogut", "Andrew Harrison", 
                                           "Andrew Wiggins", "Ante Zizic", "Anthony Davis", "Anthony Tolliver", 
                                           "Antonio Blakeney", "Aron Baynes", "Arron Afflalo", "Austin Rivers", 
                                           "Avery Bradley", "Bam Adebayo", "Ben Simmons", "Bismack Biyombo", 
                                           "Blake Griffin", "Boban Marjanovic", "Bobby Brown", "Bogdan Bogdanovic", 
                                           "Bojan Bogdanovic", "Bradley Beal", "Brandan Wright", "Brandon Ingram", 
                                           "Brandon Paul", "Brice Johnson", "Brook Lopez", "Bruno Caboclo", 
                                           "Bryn Forbes", "Buddy Hield", "Caleb Swanigan", "Caris LeVert", 
                                           "Carmelo Anthony", "Cedi Osman", "Chandler Parsons", "Channing Frye", 
                                           "Charles Cooke", "Cheick Diallo", "Chris McCullough", "Chris Paul", 
                                           "CJ McCollum", "CJ Miles", "Clint Capela", "Cody Zeller", "Cole Aldrich", 
                                           "Corey Brewer", "Cory Joseph", "Courtney Lee", "Cristiano Felicio", 
                                           "Dakari Johnson", "Damian Lillard", "Damien Wilkins", "Damyean Dotson", 
                                           "D'Angelo Russell", "Daniel Theis", "Danilo Gallinari", "Danny Green", 
                                           "Dante Cunningham", "Dario Saric", "Darius Miller", "Darrell Arthur", 
                                           "Darren Collison", "David Nwaba", "David West", "Davis Bertans", 
                                           "De'Aaron Fox", "DeAndre' Bembry", "DeAndre Jordan", "DeAndre Liggins", 
                                           "Dejounte Murray", "Delon Wright", "DeMarcus Cousins", "DeMar DeRozan", 
                                           "DeMarre Carroll", "Demetrius Jackson", "Dennis Schroder", "Dennis Smith Jr.", 
                                           "Denzel Valentine", "Derrick Favors", "Derrick Jones Jr.", "Derrick Rose", 
                                           "Derrick White", "Devin Booker", "Devin Harris", "Dewayne Dedmon", 
                                           "Deyonta Davis", "Dillon Brooks", "Dion Waiters", "Dirk Nowitzki", 
                                           "D.J. Augustin", "D.J. Wilson", "Domantas Sabonis", "Donovan Mitchell", 
                                           "Dorian Finney-Smith", "Doug McDermott", "Dragan Bender", "Draymond Green", 
                                           "Dwayne Bacon", "Dwight Howard", "Dwight Powell", "Dwyane Wade", 
                                           "Ed Davis", "Ekpe Udoh", "Elfrid Payton", "Emmanuel Mudiay", 
                                           "Enes Kanter", "Eric Bledsoe", "Eric Gordon", "Eric Moreland", 
                                           "Ersan Ilyasova", "E'Twaun Moore", "Evan Fournier", "Evan Turner", 
                                           "Frank Kaminsky", "Frank Mason", "Frank Ntilikina", "Fred VanVleet", 
                                           "Garrett Temple", "Gary Harris", "Gary Payton II", "George Hill", 
                                           "Georgios Papagiannis", "Gian Clavell", "Giannis Antetokounmpo", 
                                           "Goran Dragic", "Gordon Hayward", "Gorgui Dieng", "Greg Monroe", 
                                           "Guerschon Yabusele", "Harrison Barnes", "Hassan Whiteside", 
                                           "Henry Ellenson", "Ian Clark", "Ian Mahinmi", "Ike Anigbogu", 
                                           "Iman Shumpert", "Isaiah Canaan", "Isaiah Taylor", "Isaiah Whitehead", 
                                           "Ish Smith", "Ivica Zubac", "Jabari Bird", "Jacob Wiley", "Jae Crowder", 
                                           "Jahlil Okafor", "JaKarr Sampson", "Jake Layman", "Jakob Poeltl", 
                                           "Jalen Jones", "Jamal Crawford", "Jamal Murray", "Jameer Nelson", 
                                           "James Ennis III", "James Harden", "James Johnson", "JaMychal Green", 
                                           "Jared Dudley", "Jarell Martin", "Jarrett Allen", "Jarrett Jack", 
                                           "Jason Smith", "Jason Terry", "JaVale McGee", "Jawun Evans", 
                                           "Jaylen Brown", "Jayson Tatum", "Jeff Green", "Jeff Teague", 
                                           "Jeff Withey", "Jerami Grant", "Jeremy Lamb", "Jeremy Lin", "Jerian Grant", 
                                           "Jerryd Bayless", "Jimmy Butler", "J.J. Barea", "JJ Redick", 
                                           "Jodie Meeks", "Joe Harris", "Joe Ingles", "Joe Johnson", "Joel Embiid", 
                                           "Joe Young", "Joffrey Lauvergne", "John Collins", "John Henson", 
                                           "Johnny O'Bryant III", "John Wall", "Jonas Jerebko", "Jonas Valanciunas", 
                                           "Jonathan Isaac", "Jonathon Simmons", "Jon Leuer", "Jordan Bell", 
                                           "Jordan Clarkson", "Jordan Crawford", "Jordan Mickey", "Jose Calderon", 
                                           "Josh Hart", "Josh Huestis", "Josh Jackson", "Josh Magette", 
                                           "Josh Richardson", "Josh Smith", "JR Smith", "Jrue Holiday", 
                                           "Juan Hernangomez", "Julius Randle", "Julyan Stone", "Justin Anderson", 
                                           "Justin Holiday", "Justin Jackson", "Justise Winslow", "Jusuf Nurkic", 
                                           "Karl-Anthony Towns", "Kay Felder", "Kelly Olynyk", "Kelly Oubre Jr.", 
                                           "Kemba Walker", "Kenneth Faried", "Kentavious Caldwell-Pope", 
                                           "Kent Bazemore", "Kevin Durant", "Kevin Love", "Kevon Looney", 
                                           "Khem Birch", "Khris Middleton", "Klay Thompson", "Kosta Koufos", 
                                           "Kris Dunn", "Kristaps Porzingis", "Kyle Anderson", "Kyle Korver", 
                                           "Kyle Kuzma", "Kyle Lowry", "Kyle O'Quinn", "Kyrie Irving", "LaMarcus Aldridge", 
                                           "Lance Stephenson", "Lance Thomas", "Langston Galloway", "Larry Nance Jr.", 
                                           "Lauri Markkanen", "LeBron James", "Lonzo Ball", "Lou Williams", 
                                           "Lucas Nogueira", "Luc Mbah a Moute", "Luke Babbitt", "Luke Kennard", 
                                           "Luol Deng", "Malachi Richardson", "Malcolm Brogdon", "Malcolm Delaney", 
                                           "Malik Beasley", "Malik Monk", "Mangok Mathiang", "Manu Ginobili", 
                                           "Marc Gasol", "Marcin Gortat", "Marco Belinelli", "Marcus Georges-Hunt", 
                                           "Marcus Morris", "Marcus Paige", "Marcus Smart", "Mario Chalmers", 
                                           "Mario Hezonja", "Markelle Fultz", "Markieff Morris", "Marquese Chriss", 
                                           "Marreese Speights", "Marvin Williams", "Mason Plumlee", "Matthew Dellavedova", 
                                           "Maurice Harkless", "Maxi Kleber", "Meyers Leonard", "Michael Beasley", 
                                           "Michael Kidd-Gilchrist", "Mike Conley", "Mike James", "Mike Muscala", 
                                           "Mike Scott", "Milos Teodosic", "Mirza Teletovic", "Montrezl Harrell", 
                                           "Myles Turner", "Nemanja Bjelica", "Nene", "Nerlens Noel", "Nick Young", 
                                           "Nicolas Brussino", "Nikola Jokic", "Nikola Vucevic", "Nik Stauskas", 
                                           "Noah Vonleh", "Norman Powell", "OG Anunoby", "Okaro White", 
                                           "Omri Casspi", "Otto Porter Jr.", "Pascal Siakam", "Pat Connaughton", 
                                           "Patrick Beverley", "Patrick McCaw", "Patrick Patterson", "Patty Mills", 
                                           "Pau Gasol", "Paul George", "Paul Millsap", "Paul Zipser", "PJ Tucker", 
                                           "Quincy Acy", "Quincy Pondexter", "Quinn Cook", "Ramon Sessions", 
                                           "Rashad Vaughn", "Raul Neto", "Raymond Felton", "Reggie Bullock", 
                                           "Reggie Jackson", "Richard Jefferson", "Richaun Holmes", "Ricky Rubio", 
                                           "Robert Covington", "Robin Lopez", "Rodney Hood", "Ron Baker", 
                                           "Rondae Hollis-Jefferson", "Royce O'Neale", "Rudy Gay", "Rudy Gobert", 
                                           "Russell Westbrook", "Ryan Anderson", "Ryan Arcidiacono", "Salah Mejri", 
                                           "Sam Dekker", "Sean Kilpatrick", "Semi Ojeleye", "Serge Ibaka", 
                                           "Shabazz Muhammad", "Shabazz Napier", "Shane Larkin", "Shaun Livingston", 
                                           "Shelvin Mack", "Sindarius Thornwell", "Skal Labissiere", "Spencer Dinwiddie", 
                                           "Stanley Johnson", "Stephen Curry", "Sterling Brown", "Steven Adams", 
                                           "Taj Gibson", "Tarik Black", "Taurean Prince", "Terrance Ferguson", 
                                           "Terrence Ross", "Terry Rozier", "Thabo Sefolosha", "Thaddeus Young", 
                                           "Thon Maker", "Tim Frazier", "Tim Hardaway Jr.", "Timofey Mozgov", 
                                           "Timothe Luwawu-Cabarrot", "TJ Leaf", "T.J. McConnell", "TJ Warren", 
                                           "Tobias Harris", "Tomas Satoransky", "Tony Allen", "Tony Snell", 
                                           "Treveon Graham", "Trevor Ariza", "Trevor Booker", "Trey Lyles", 
                                           "Tristan Thompson", "Troy Daniels", "Troy Williams", "Tyler Dorsey", 
                                           "Tyler Ennis", "Tyler Johnson", "Tyler Ulis", "Tyler Zeller", 
                                           "Tyreke Evans", "Tyson Chandler", "Tyus Jones", "Victor Oladipo", 
                                           "Vince Carter", "Wayne Ellington", "Wes Iwundu", "Wesley Johnson", 
                                           "Wesley Matthews", "Will Barton", "Willie Cauley-Stein", "Willie Reed", 
                                           "Willy Hernangomez", "Wilson Chandler", "Yogi Ferrell", "Zach Collins", 
                                           "Zach Randolph", "Zaza Pachulia", "Zhou Qi"),
                              selected = "James Harden"),
                  selectInput(inputId = "playType",
                              label = "Made, Missed or Both:",
                              choices = c("Made", "Missed", "Both"),
                              selected = "Both"
                  )                  
                ),
                mainPanel(plotOutput("playPlotPoint"),
                          downloadLink("downloadPlayer", "Download"))
              ))
              )
))