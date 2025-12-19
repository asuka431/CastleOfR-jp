#' R6 classes for the Castle of R game

Room <- R6::R6Class("Room",
                public = list(
                  name = NULL,
                  title = NULL,
                  door = NULL,
                  object = NULL,
                  timeLimit = Inf,
                  object_numbers = NULL,
                  floor = NULL,
                  additionalCommentToGreetMessage = NULL,
                  initialize = function(name = NA, title = NA, floor = NA,
                                        comment = NA) {
                    self$name <- name
                    self$title <- title
                    self$floor <- floor
                    self$additionalCommentToGreetMessage <- comment
                    # self$additionalCommentToGreetMessage <- ifelse(comment == "NoComment",
                    #                                                "", comment)
                  },
                  set_doors = function(doors) {
                    self$door <- doors
                  },
                  set_objects = function(objects) {
                    self$object <- objects
                    self$object_numbers <- 1:length(objects)
                  },
                  set_timeLimit = function(timeLimit) {
                    self$timeLimit <- as.numeric(timeLimit)
                  },
                  set_additionalCommentToGreetMessage = function(comment) {
                    self$additionalCommentToGreetMessage <- comment
                  },
                  countLockedDoors = function() {
                    doors <- self$door
                    if (is.null(doors)) return(0)
                    # if a single Door object was set, wrap it into a list for iteration
                    if (inherits(doors, "Door")) doors <- list(doors)
                    # use vapply to guarantee a logical(1) result for each element
                    sum(vapply(doors, function(door) !isTRUE(door$open), logical(1)))
                  },
                  doorsList_toString = function(directionChosen) {
                    l <- self$door
                    s <- ""
                    if (!is.null(l) && is.list(l) && length(l) > 0) {
                      s <- paste0(s, "You see ")
                      if (length(l) == 1) {
                        s <- paste0(s,
                                    l[[1]]$toString(self$name, directionChosen),
                                    " (1).")
                      } else {
                        doorStrings <- lapply(l,
                                              function(x)
                                                x$toString(self$name, directionChosen))
                        numbers <- 1:length(l)
                        s <- paste0(s,
                                    paste0(
                                      paste0(
                                        doorStrings[-length(l)],
                                        paste0(
                                          " (", numbers[-length(l)], ")"),
                                        collapse = ", "),
                                      " and ", doorStrings[length(l)],
                                      " (", numbers[length(l)], ")."))
                      }
                    }
                    return(s)
                  },
                  objectsList_toString = function() {
                    l <- self$object
                    numbers <- self$object_numbers
                    s <- ""
                    if (!is.null(l) && is.list(l) && length(l) > 0) {
                      takenObjectsIdx <- which(sapply(l, function(obj) !obj$taken))
                      l <- l[takenObjectsIdx]
                      numbers <- numbers[takenObjectsIdx]
                      if (length(l) > 0) {
                        s <- paste0(s, "Around you, you see ")
                        if (length(l) == 1) {
                          s <- paste0(s, l[[1]]$toString(), " (", numbers[1], ").")
                        } else {
                          objStrings <- lapply(l, function(x) x$toString())
                          s <- paste0(s,
                                      paste0(
                                        paste0(
                                          objStrings[-length(l)],
                                          paste0(
                                            " (", numbers[-length(l)], ")"),
                                          collapse = ", "),
                                        " and ", objStrings[length(l)],
                                        " (", numbers[length(l)], ")."))
                        }
                      }
                    }
                    return(s)
                  },
                  greet = function(directionChosen = NULL) {
                    floorNum <- switch(self$floor, "1" = "1st", "2" = "2nd",
                                       "3" = "3rd", "4" = "4th")
                    objectsString <- self$objectsList_toString()
                    doorsString <- self$doorsList_toString(directionChosen)
                    message(paste0("You are in ", self$title, ", ", floorNum,
                                   " floor.\n",
                                   objectsString,
                                   ifelse(objectsString == "", doorsString, paste0("\n", doorsString)),
                                   ifelse(is.na(self$additionalCommentToGreetMessage), "",
                                                paste0("\n", self$additionalCommentToGreetMessage))))
                  }
                )
)

Door <- R6::R6Class("Door",
                public = list(
                  direction = list(NULL, NULL),
                  room = list(NULL, NULL),
                  riddle = list(NULL, NULL),
                  open = FALSE,
                  initialize = function(direction = NA,
                                        room = list(NA, NA),
                                        riddle = list(NA, NA)) {
                    if (!direction %in% c("north", "south", "east", "west", "up", "down")) {
                      stop("invalid direction")
                    }
                    self$direction[[1]] <- direction
                    self$direction[[2]] <- self$reverseDirection(self$direction[[1]])
                    self$room <- room
                    self$riddle <- riddle
                    names(self$riddle) <- self$getRoomsNames()
                    names(self$direction) <- self$getRoomsNames()
                  },
                  getRoomsNames = function() {
                    lapply(self$room, function(room) room$name)
                  },
                  openDoor = function() {
                    self$open <- TRUE
                  },
                  lockDoor = function() {
                    self$open <- FALSE
                  },
                  reverseDirection = function(direction) {
                    switch(direction,
                           "north" = "south",
                           "south" = "north",
                           "east" = "west",
                           "west" = "east",
                           "up" = "down",
                           "down" = "up")
                  },
                  getRiddle = function(room) {
                    self$riddle[[room]]
                  },
                  getRoom = function(room) {
                    self$room[[room]]
                  },
                  getNextRoom = function(room) {
                    self$room[[which(self$getRoomsNames() != room)]]
                  },
                  getDirection = function(room) {
                    self$direction[[room]]
                  },
                  toString = function(room, directionChosen) {
                    currentDirection <- self$getDirection(room)
                    s <- switch(currentDirection,
                           "north" = "a door to the north",
                           "south" = "a door to the south",
                           "east" = "a door to the east",
                           "west" = "a door to the west",
                           "up" = "a hatch in the ceiling to an upper floor",
                           "down" = "a hatch on the ground to a lower floor")
                    if (!is.null(directionChosen)) {
                      reverseDirectionChosen <- self$reverseDirection(directionChosen)
                      if (reverseDirectionChosen == currentDirection) {
                        s <- paste0(s, " (where you just came from)")
                      }
                    }
                    return(s)
                  }
                ))

Object <- R6::R6Class("Object",
                  public = list(
                    name = NULL,
                    location = NULL,
                    riddle = NULL,
                    taken = FALSE,
                    type = NULL,
                    points = NULL,
                    initialize = function(name = NA, location = NA, type = NA,
                                          points = NA, riddle = NA) {
                      self$name <- name
                      self$location <- location
                      self$type <- type
                      self$points <- as.numeric(points)
                      self$riddle <- riddle
                    },
                    toString = function() {
                      prefix <- ifelse(grepl("^[aeiou]", self$name), "an", "a")
                      paste0(prefix, " ", self$name, " ", self$location)
                    },
                    takeObject = function() {
                      self$taken <- TRUE
                    }
                  ))

Riddle <- R6::R6Class("Riddle",
                  public = list(
                    question = NULL,
                    solution = NULL,
                    val = NULL,
                    hint = NULL,
                    tip = NULL,
                    floorMapsIdx = NULL,
                    prepare = NULL,
                    cleanup = NULL,
                    initialize = function(question = NA, solution = NA, val = NA,
                                          hint = NA, tip = NA, floorMapsIdx = NA,
                                          prepare = NA, cleanup = NA) {
                      self$question <- question
                      self$solution <- solution
                      self$val <- tryCatch(as.numeric(val),
                                           warning = function(w) val)
                      self$hint <- hint
                      self$tip <- tip
                      self$floorMapsIdx <- floorMapsIdx
                      self$prepare <- prepare
                      self$cleanup <- cleanup
                    },
                    askQuestion = function() {
                      message(self$question)
                    },
                    getSolution = function() {
                      message(self$solution)
                    },
                    getHint = function() {
                      message(self$hint)
                    }
                  ))


TimeRoom <- R6::R6Class("TimeRoom",
                    inherit = Room,
                    public = list(
                      riddle = NULL,
                      timeLimit = NULL,
                      floorMapsIdx = NULL,
                      initialize = function(name = NA, title = NA, timeLimit = NA, floorMapsIdx = NA) {
                        self$name <- name
                        self$title <- title
                        self$timeLimit <- as.numeric(timeLimit)
                        self$floorMapsIdx <- floorMapsIdx
                      },
                      set_riddles = function(riddles) {
                        self$riddle <- riddles
                      },
                      set_timeLimit = function(timeLimit) {
                        invisible()
                      },
                      greet = function(alreadyHasMap = FALSE) {
                        message(paste0("Oh oh. You reached ", self$title,
                                       ".\n\nThis means you have ",
                                       self$timeLimit, " minutes to answer these ",
                                       length(self$riddle),
                                       " questions.\nIf you don't make it - Lady R will get you.\nBut if you do - you will return to the previous room ",
                                       ifelse(!alreadyHasMap, "with a valuable\npiece of information.",
                                              "(with nothing 'cause you've already been here!)."), "\n"))
                      }
                    )
)

DarkRoom <- R6::R6Class("DarkRoom",
                    inherit = Room,
                    public = list(
                      nObjectsLeave = NULL,
                      #timeLimit = Inf,
                      initialize = function(name = NA, title = NA, nObjectsLeave = NA) {
                        self$name <- name
                        self$title <- title
                        self$nObjectsLeave <- as.numeric(nObjectsLeave)
                      },
                      greet = function(directionChosen = NULL) {
                        message(paste0("Damn! You reached ", self$title,
                                       "!\n\nYou can't see a thing.\n\nSuddenly, you see a small candle light approaching towards you.\nOh no! It's Lady R! She's laughing maniacally and is coming towards you!\n\nQuick! To go back you must leave behind ",
                                       self$nObjectsLeave, " objects. Do you have enough stuff in your satchel to go back?"))
                      }
                    )
)

gameStartScenario <- function() {
  message("Castle of R! へ ようこそ！")
  message("\nCastle of Rは 'An Introduction to R' 著者の Venables, Smith と R Core Team によるテキストベース な アドベンチャーです。 このゲームはRの基本能力を確認するためのものです。")
  message("\nゲーム中にendGame()と入力すれば(基本的に)いつでも中断して退出が可能です。それでは始まりです。")
  message("\nあなたは城の1階、ロビーにいます。\nあなたの正面には、アール城の持ち主である アール婦人 というやさしそうな老婦人が座っています。\nあなたの後ろの窓からはアール城の美しい庭園が見えることでしょう。\n正面のアール婦人の肩越しに、北側に続くドアが見えます。")
  message("アール婦人は貴方に名前を訪ねます。")
  playerName <- readline()
  message(paste0("\"お会いできてとっても嬉しいわ ", playerName, ". お茶はいかがかしら?\""))
  tea <- menu(c("yes", "no")) == 1
  if (tea) {
    message("アール婦人はカップにお茶を注ぐ。お茶を飲むと、カップの底に奇妙なメッセージが見えた。...\n      \"ヨメ\"\n")
  }
  message("Lady R says:\"ビスケットもどうぞ?\"")
  biscuits <- menu(c("yes", "no")) == 1
  if (biscuits) {
    message("アール婦人は貴方にビスケットの小皿を勧める。 貴方はビスケットを食べ進めると、皿の真ん中に有るメッセージが見えた。...\n      \"ハシレ\"")
  } else {
    message("\"とってもお行儀がよろしいですね?まったく...\"")
  }
  message("\n突然、アール婦人が椅子から立ち上がった。\n先程の優しさはどこへやら。彼女は包丁を手に取り、言い放った。\n\"今日はとってもいい日よね。新鮮なRユーザーがのこのこと迷い込んできた!\"\n貴方は状況を察した。逃げなくては。 しかしどこへ?")
  message("\nあなたはバックパックを抱えて北のドアに向かって走り出す。急いで\"openDoor(1)\"と入力しろ! 早く!")
  startTime <- Sys.time()
}

Lounge <- R6::R6Class("ロビー",
                  inherit = Room,
                  public = list(
                    startScenario = function() {
                      gameStartScenario()
                    }
                  )
)

Bridge <- R6::R6Class("橋",
                      inherit = Room,
                      public = list(
                        greet = function(directionChosen = NULL) {
                          doorsString <- self$doorsList_toString(directionChosen)
                          message(paste0("貴方は ", self$title,
                                         " をくぐった。\n", doorsString))
                        }
                      )
)