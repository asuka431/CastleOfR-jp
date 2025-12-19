#' Initialize the Castle of R game environment
#' 
#' This function reads in the various text files containing the data and
#' configuration of the various game components such as rooms, doors, objects
#' and riddles.
initializeGame <- function(playerLevel) {
  
  game <- new.env(globalenv())
    
  #lounge
  lounge <- Lounge$new("ロビー", "ロビー", "1", NA)
  lounge$set_objects(list(Object$new("ティーカップ", "テーブルの上", "R力", 3,
                                     Riddle$new("0 + 0は?", "0 + 0", 0,
                                                "0..."))))
  
  #bridge
  bridge <- Bridge$new("橋", "運命の橋", "4", NA)
  
  # rooms
  rooms_file <- system.file("extdata", "CastleOfR_Rooms.txt",
                            package = "CastleOfR")
  rooms_df <- read.table(rooms_file, stringsAsFactors = FALSE, header = TRUE,
                         sep = "\t", comment.char = "|")
  rooms_list <- apply(rooms_df, 1, function(room) Room$new(room[["name"]],
                                                           room[["title"]],
                                                           room[["floor"]],
                                                           room[["comment"]]))
  names(rooms_list) <- rooms_df$name
  list2env(rooms_list, envir = environment())
  
  # time rooms
  timeRooms_file <- system.file("extdata", "CastleOfR_TimeRooms.txt",
                                package = "CastleOfR")
  timeRooms_df <- read.table(timeRooms_file, stringsAsFactors = FALSE,
                             header = TRUE, sep = "\t", comment.char = "|")
  timeRooms_list <- apply(timeRooms_df, 1,
                          function(room) TimeRoom$new(room[["name"]],
                                                      room[["title"]],
                                                      room[["timeLimit"]],
                                                      room[["floorMapsIdx"]]))
  names(timeRooms_list) <- timeRooms_df$name
  list2env(timeRooms_list, envir = environment())
  
  # dark rooms
  darkRooms_file <- system.file("extdata", "CastleOfR_DarkRooms.txt",
                                package = "CastleOfR")
  darkRooms_df <- read.table(darkRooms_file, stringsAsFactors = FALSE,
                             header = TRUE, sep = "\t", comment.char = "|")
  darkRooms_list <- apply(darkRooms_df, 1, function(room) 
    DarkRoom$new(room[["name"]], room[["title"]], room[["nObjectsLeave"]]))
  names(darkRooms_list) <- darkRooms_df$name
  list2env(darkRooms_list, envir = environment())
  
  # doors
  doors_file <- system.file("extdata", "CastleOfR_Doors.txt",
                            package = "CastleOfR")
  doors_df <- read.table(doors_file, stringsAsFactors = FALSE, header = TRUE,
                         sep = "\t", comment.char = "|")
  
  doors_list <- apply(doors_df, 1, function(door) {
    riddle1 <- Riddle$new(door[["question1"]], door[["solution1"]],
                          door[["val1"]], door[["hint1"]],
                          NA, NA,
                          door[["prepare1"]], door[["cleanup1"]])
    riddle2 <- Riddle$new(door[["question2"]], door[["solution2"]],
                          door[["val2"]], door[["hint2"]],
                          NA, NA,
                          door[["prepare2"]], door[["cleanup2"]])
    Door$new(door[["direction_1to2"]],
             list(get(door[["room1"]]), get(door[["room2"]])),
             list(riddle1, riddle2))
  })
  names(doors_list) <- doors_df$name
  list2env(doors_list, envir = environment())
  
  # objects
  objects_file <- system.file("extdata", "CastleOfR_Objects.txt",
                              package = "CastleOfR")
  objects_df <- read.table(objects_file, stringsAsFactors = FALSE,
                           header = TRUE, sep = "\t", comment.char = "|")
  
  objects_list <- apply(objects_df, 1, function(object) {
    riddle <- Riddle$new(object[["question"]], object[["solution"]],
                         object[["val"]], object[["hint"]], object[["tip"]],
                         object[["floorMapsIdx"]], object[["prepare"]],
                         object[["cleanup"]])
    Object$new(object[["objName"]], object[["location"]], object[["type"]],
               object[["points"]], riddle)
  })
  names(objects_list) <- objects_df$name
  list2env(objects_list, envir = environment())
  
  # time rooms riddles
  trRiddles_file <- system.file("extdata", "CastleOfR_TimeRoomsRiddles.txt",
                                package = "CastleOfR")
  trRiddles_df <- read.table(trRiddles_file, stringsAsFactors = FALSE,
                             header = TRUE, sep = "\t", comment.char = "|")
  trRiddles_list <- apply(trRiddles_df, 1, function(trRiddle)
    Riddle$new(trRiddle[["question"]], trRiddle[["solution"]],
               trRiddle[["val"]], trRiddle[["hint"]],
               NA, NA,
               trRiddle[["prepare"]], trRiddle[["cleanup"]]))
  names(trRiddles_list) <- trRiddles_df$name
  list2env(trRiddles_list, envir = environment())
  
  # maps
  mapsNames <- c("CastleOfR_floor1.png", "CastleOfR_floor2.png", "CastleOfR_floor3.png", "CastleOfR_Towers.png")
  mapsNames <- sapply(mapsNames, function(mapName) system.file("extdata", mapName, package = "CastleOfR"))
  floorMapsAvailable <- lapply(mapsNames, png::readPNG)
  names(floorMapsAvailable) <- 1:4
  
  # set doors to rooms
  getDoorsObjectsForRoom <- function(l, roomName) {
    mget(names(l)[which(grepl(roomName, names(l)))], inherits = TRUE)
  }
  invisible(lapply(rooms_list, function(room) room$set_doors(getDoorsObjectsForRoom(doors_list, room$name))))
  invisible(lapply(darkRooms_list, function(room) room$set_doors(getDoorsObjectsForRoom(doors_list, room$name))))
  invisible(lapply(timeRooms_list, function(room) room$set_doors(getDoorsObjectsForRoom(doors_list, room$name))))
  lounge$set_doors(getDoorsObjectsForRoom(doors_list, lounge$name))
  bridge$set_doors(getDoorsObjectsForRoom(doors_list, bridge$name))
  
  # set objects to rooms
  invisible(lapply(rooms_list, function(room) room$set_objects(getDoorsObjectsForRoom(objects_list, room$name))))
  
  # set timeLimits to regular rooms
  game$playerLevel <- playerLevel
  
  roomTimeLimit <- switch(game$playerLevel,
                          "1" = 1,
                          "2" = 3,
                          "3" = 5,
                          "4" = 10)
  lockedDoorDelay <- roomTimeLimit / 5
  invisible(lapply(rooms_list, function(room) room$set_timeLimit(roomTimeLimit +
                                                                   lockedDoorDelay *
                                                                   room$countLockedDoors())))
  lounge$set_timeLimit(roomTimeLimit +
                         lockedDoorDelay * lounge$countLockedDoors() + 5)
  
  # set riddles to time rooms
  invisible(lapply(timeRooms_list, function(room) room$set_riddles(getDoorsObjectsForRoom(trRiddles_list, room$name))))
  
  # set password (no no uppercase I, lowercase L)
  pwd <- sample(c(LETTERS[-9], letters[-12], 0:9), 7, replace = TRUE)
  
  # game internal functions
  compareExpression <- function(targetExp, regex = FALSE) {
    if (regex) {
      length(game$deparsedExpr) == 1 && grepl(targetExp, game$deparsedExpr)
    } else {
      !is.null(game$expr) && length(game$deparsedExpr) == 1 &&
        game$deparsedExpr != "NA" && game$expr == targetExp
    }
  }
  
  wtf <- function() {
    game$currentRoom$greet()
    game$whatDoIHave()
    game$timeLeft()
  }
  
  whatDoIHave <- function() {
    if (length(game$satchel) > 0) {
      message(paste0("バックパックには... ",
                     paste(lapply(game$satchel, function(obj) obj$name),
                           collapse = ", "),
                     "."))
    } else {
      "バックパックには...何もない!."
    }
    message(paste0("あなたのR力は", game$RPower, " pointだ。"))
    if (length(game$floorMapsPlayer) > 0) {
      if (length(game$floorMapsPlayer) == 1) {
        message(paste0("そして ", names(game$floorMapsPlayer)[1], "階の地図を持っている"))
      } else {
        message(paste0("そして",
                       paste0(names(game$floorMapsPlayer), collapse = ", "), "階の地図を持っている"))
      }
    } else {
      message("そして地図は持っていない。")
    }
  }
  
  whatWasTheQuestion <- function() {
    if (!is.null(game$riddle)) {
      game$riddle$askQuestion()
    } else {
      message("問題は まだ 出ていない。")
    }
  }
  
  removeNObjectsFromSatchel <- function(n) {
    for (obj in 1:n) {
      message(paste0("捨てるものを選んでください ", obj, ":"))
      objIdx <- menu(lapply(game$satchel, function(obj) obj$name))
      game$satchel <- game$satchel[-objIdx]
    }
  }
  
  endGame <- function(endMessage = NULL, requestedByGame = TRUE) {
    if (!requestedByGame && class(game$currentRoom)[1] %in% c("TimeRoom", "DarkRoom")) {
      message("この部屋ではゲーム終了は不可能だ。")
      return(FALSE)
    } else {
      message(endMessage)
      if (is.null(game$mode) || !game$mode %in% c("time", "dark", "lose", "win")) {
        message("セーブして、中断したところから再開できるようにする?")
        saveAns <- menu(c("yes", "no")) == 1
        if (saveAns) {
          saveRDS(game, file.path(find.package("CastleOfR"), "CastleOfR_game.RData"))
        }
      }
      message("終了前にワークスペースとプロットを削除する?")
      cleanAns <- menu(c("yes", "no")) == 1
      if (cleanAns) {
        graphics.off()
        tryCatch(rm(list = ls(envir = globalenv()), envir = globalenv()),
                 warning = function(w) {invisible()})
      }
      rm(.gameOn, envir = globalenv())
      removeTaskCallback("CastleOfR")
      return(TRUE)
    }
  }
  
  plotMap <- function(map) {
    plot(1:2, type = "n", main = "", xlab = "", ylab = "", bty = "n",
         xaxt = "n", yaxt = "n")
    lim <- par()
    rasterImage(map, lim$usr[1], lim$usr[3], lim$usr[1] +
                  (dim(map)[1]/dim(map)[2]) *(lim$usr[2] - lim$usr[1]), lim$usr[4])
  }
  
  seeMap <- function() {
    map_idx <- as.numeric(unlist(regmatches(game$deparsedExpr,
                                            gregexpr("[0-9]+",
                                                     game$deparsedExpr))))
    if (map_idx > 0 && map_idx <= length(game$floorMapsAvailable)) {
      if (toString(map_idx) %in% names(game$floorMapsPlayer)) {
        game$plotMap(game$floorMapsPlayer[[toString(map_idx)]])
      } else {
        message(map_idx, "階の地図は持っていない。")
      }
    } else {
      message("そんな階はない。")
    }
  }
  
  plotPwd <- function(...) {
    defaultBGColor <- par(bg = "black")
    plot(1:length(game$pwd), 1:7, type = "n", main = "", xlab = "", ylab = "",
         bty = "n", xaxt = "n", yaxt = "n", ...)
    for (i in game$pwdExposedIdx) text(i, 4, labels = game$pwd[i],
                                       col = "white", cex = 3)
    on.exit(par(defaultBGColor))
  }
  
  isObjectInSatchel <- function(objName) {
    if (length(game$satchel) > 0) {
      objName %in% sapply(game$satchel, function(obj) obj$name)
    } else {
      FALSE
    }
  }
  
  wasObjectInSatchel <- function(objName) {
    if (length(game$satchelHist) > 0) {
      objName %in% sapply(game$satchelHist, function(obj) obj$name)
    } else {
      FALSE
    }
  }
  
  teacupScenario <- function() {
    message("\"私のティーカップ持ってる?\"")
    ansTeacup <- menu(c("yes", "no", "確認する"))
    isTeacup <- isObjectInSatchel("teacup")
    wasTeacup <- wasObjectInSatchel("teacup")
    if (ansTeacup == 1) {
      if (isTeacup) {
        game$winScenario()
        return(TRUE)
      } else {
        if (wasTeacup) {
          game$loseScenario("\"当然だ! 暗闇の間かどこかでポイしちゃったんだから!\"\nアールドラゴンは翼を翻し、あなたをおいて飛び去っていった...。")
          return(TRUE)
        } else {
          message("嘘じゃん! もしこの城から逃げたいなら、私のティーカップを取りに行くことをお勧めするね！")
          return(FALSE)
        }
      }
    } else if (ansTeacup == 2) {
      if (isTeacup) {
        game$winScenario()
        return(TRUE)
      } else {
        if (wasTeacup) {
          game$loseScenario("\"当然だ! 暗闇の間かどこかでポイしちゃったんだから!\"\nアールドラゴンは翼を翻し、あなたをおいて飛び去っていった...。")
          return(TRUE)
        } else {
          message("\"そう、もしこの城から逃げたいなら、私のティーカップを取りに行くことをお勧めするね！\"そう言いながらアールドラゴンは飛び去っていった。")
          return(FALSE)
        }
      }
    } else {
      message("\"そうして。準備ができたらもう一度アールドラゴン召喚を試してね。\"そう言いながらアールドラゴンは飛び去っていった。")
      return(FALSE)
    }
  }
  
  summonRDragon <- function() {
    if (game$escapeRoom$name == game$currentRoom$name) {
      if (game$dragonSeen) {
        message("アールドラゴンは再び城の屋根に降り立った。")
      } else {
        game$dragonSeen <- TRUE
        message("塔の窓から見える。")
        message("雄大な竜がこちらへ飛んでくる。あなたはおとぎ話を信じる人では無いのだが...")
        message("\nアールドラゴンだ!\nドラゴンは城の屋根に降り立ち、あなたを見下ろしながら。重々しく言った。")
      }
      message("\"合言葉は。\"")
      #message(paste0("pwd is: ", paste0(game$pwd, collapse = "")))
      inputPwd <- readline()
      if (game$isPasswordCorrect(inputPwd)) {
        message("そのとおり。")
        return(game$teacupScenario())
      } else {
        message("\"合言葉が違う。アールドラゴンに嘘をつくなんて信じらんない?!\" アールドラゴンは飛び去った。")
        return(FALSE)
      }
    } else {
      message("外に出られる場所に行かなくては。")
      return(FALSE)
    }
  }
  
  isPasswordCorrect <- function(inputPwd) {
    inputPwdSplit <- strsplit(inputPwd, "")[[1]]
    identical(inputPwdSplit, game$pwd)
  }
  
  hintSolution <- function(type) {
    if (!is.null(game$riddle)) {
      RPowerCost <- ifelse(type == "hint", game$hintRPower, game$solutionRPower)
      if (game$RPower >= RPowerCost) {
        message(paste0("本当にこの ",
                       type, " にR力を使用する?(", RPowerCost, " points)"))
        payRPower <- menu(c("yes", "no")) == 1
        if (payRPower) {
          if(type == "hint") {
            message(paste0("ヒント: ", game$riddle$hint))
          } else {
            message(paste0("解法: ", game$riddle$solution))
          }
          game$RPower <- game$RPower - RPowerCost
        }
      } else {
        message(paste0("R力が足りない!この", type, " には(",
                       RPowerCost, " point)必要だ。"))
      }
    } else {
      message("今問題は出ていない。")
    }
  }
  
  timeLeft <- function() {
    message(paste0("アール婦人はこの部屋にあと ",
                   strftime(
                     as.POSIXct(
                       as.numeric(
                         difftime(game$roomStartTime +
                                    60 * game$currentRoom$timeLimit,
                                  Sys.time(), units = "sec")),
                       origin = Sys.Date()),
                     format = "%M:%S"), " 分で来る！"))
  }
  
  loseScenario <- function(loseMessage = NULL) {
    message(loseMessage)
    message("ババアの狂った笑いが背後で聞こえた。")
    message("振り返ると、彼女が包丁を振っているのが見える。")
    message("\"ゲームオーバー!\"彼女は狂ったように叫んでいる。\"歩け!歩け私のR奴隷!\"")
    message("ババアはお前を城から牢獄塔へと連れて行きます。あなたは残りの人生を、激ダルい行列の乗算やデータ処理に費やすことになります。")
    message("次の犠牲者となるRユーザーが現れるまで...。")
    game$mode <- "lose"
    game$endGame()
  }
  
  winScenario <- function() {
    message("\"わー私のお気に入りのティーカップ。ドラゴン達はみんなお茶に目がないって知ってた?\"")
    message("アールドラゴンはあなたを背中に乗せてくれた。城の屋根から飛び立つあなた達の背後から、ババアの恨めしい叫びが聞こえる。\"お前は戻ってくる!どうせ皆リプレイするんだ!\"")
    message("おめでとうございます。あなたは間一髪、アールの城からの脱出に成功したようです。.\n\nまさにナイト・オブ・アールの伝説のとおりです。")
    game$mode <- "win"
    game$endGame()
    return(TRUE)
  }
  
  openDoor <- function() {
    if (class(game$currentRoom)[1] %in% c("TimeRoom", "DarkRoom")) {
      return()
    }
    game$door_idx <- as.numeric(unlist(regmatches(game$deparsedExpr,
                                                  gregexpr("[0-9]+",
                                                           game$deparsedExpr))))
    if (game$door_idx > 0 && game$door_idx <= length(game$currentRoom$door)) {
      game$nextRoom <-
        game$currentRoom$door[[game$door_idx]]$getNextRoom(game$currentRoom$name)
      if (!game$currentRoom$door[[game$door_idx]]$open) {
        game$mode <- "door"
        game$riddle <-
          game$currentRoom$door[[game$door_idx]]$getRiddle(game$currentRoom$name)
        if (!is.na(game$riddle$prepare)) {
          eval(parse(text = game$riddle$prepare))
        }
        game$riddle$askQuestion()
      } else {
        message("ドアは開いている")
        game$directionChosen <-
          game$currentRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
        game$previousRoom <- game$currentRoom
        game$currentRoom <- game$nextRoom
        game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                         game$currentRoom$countLockedDoors() *
                                         game$lockedDoorDelay)
        game$roomStartTime <- Sys.time()
        if (class(game$currentRoom)[1] == "TimeRoom") {
          game$currentRoom$greet(game$currentRoom$floorMapsIdx %in%
                                   names(game$floorMapsPlayer))
          game$mode <- "time"
          game$trRiddleIdx <- 1
          game$riddle <- game$currentRoom$riddle[[game$trRiddleIdx]]
          if (!is.na(game$riddle$prepare)) {
            eval(parse(text = game$riddle$prepare))
          }
          message(paste0("問題 ", game$trRiddleIdx, " を ",
                         length(game$currentRoom$riddle), "で割ると:"))
          game$riddle$askQuestion()
        } else if (class(game$currentRoom)[1] == "DarkRoom") {
          game$mode <- "dark"
          game$currentRoom$greet(game$directionChosen)
          if (length(game$satchel) >= game$currentRoom$nObjectsLeave) {
            # subtract necessary objects and go back to previous room
            message("正解!")
            game$removeNObjectsFromSatchel(game$currentRoom$nObjectsLeave)
            message("素晴らしい! あなたは前の部屋に引き戻される。\n")
            game$directionChosen <-
              game$previousRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
            game$currentRoom <- game$previousRoom
            game$previousRoom <- NULL
            game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                             game$currentRoom$countLockedDoors() *
                                             game$lockedDoorDelay)
            game$currentRoom$greet(game$directionChosen)
            game$roomStartTime <- Sys.time()
            game$mode <- NULL
            game$riddle <- NULL
            game$nextRoom <- NULL
          } else {
            loseMessage <- paste0("おっと、 ", ifelse(length(game$satchel) == 0,
                                                    "あなたは",
                                                    paste0("を持っていないようだ。バックパックには",
                                                           length(game$satchel))),
                                  "が入っている。")
            game$loseScenario(loseMessage)
            return(TRUE)
          }
        } else {
          game$currentRoom$greet(game$directionChosen)
          game$mode <- NULL
          game$riddle <- NULL
          game$nextRoom <- NULL
        }
      }
    } else {
      message("そんな扉はない")
    }
  }
  
  lockDoor <- function() {
    idx <- as.numeric(unlist(regmatches(game$deparsedExpr,
                                        gregexpr("[0-9]+",
                                                 game$deparsedExpr))))
    if (idx > 0 && idx <= length(game$currentRoom$door)) {
      game$currentRoom$door[[idx]]$lockDoor()
      game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                       game$currentRoom$countLockedDoors() *
                                       game$lockedDoorDelay)
      message("ドアには鍵がかかっている")
    } else {
      message("そんな扉はない")
    }
  }
  
  takeObject <- function() {
    idx <- as.numeric(unlist(regmatches(game$deparsedExpr,
                                        gregexpr("[0-9]+",
                                                 game$deparsedExpr))))
    if (idx > 0 && idx <= length(game$currentRoom$object)) {
      if (!game$currentRoom$object[[idx]]$taken) {
        game$mode <- "object"
        game$object_idx <- idx
        game$riddle <- game$currentRoom$object[[idx]]$riddle
        if (!is.na(game$riddle$prepare)) {
          eval(parse(text = game$riddle$prepare))
        }
        game$riddle$askQuestion()
      } else {
        message("アイテムを取った")
      }
    } else {
      message("そんなアイテムはない")
    }
  }
  
  reactToCall <- function() {
    if (game$deparsedExpr == game$riddle$solution ||
        (is.numeric(game$val) && !is.na(game$riddle$val) &&
         game$val == game$riddle$val)) {
      message("正解!")
      if (!is.na(game$riddle$cleanup)) {
        eval(parse(text = game$riddle$cleanup))
      }
      if (game$mode == "door") {
        game$directionChosen <- 
          game$currentRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
        
        doorOpensMessage <- switch(game$directionChosen,
                                   "up" = "上の階へのハッチが開く。",
                                   "down" = "下の階へのハッチが開く。",
                                   paste0(game$directionChosen, "へのドアが開く。"))
        message(doorOpensMessage)
        game$previousRoom <- game$currentRoom
        game$currentRoom$door[[game$door_idx]]$openDoor()
        game$currentRoom <- game$nextRoom
        game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                         game$currentRoom$countLockedDoors() *
                                         game$lockedDoorDelay)
        game$nextRoom <- NULL
        game$roomStartTime <- Sys.time()
        game$mode <- NULL
        game$riddle <- NULL
        if (class(game$currentRoom)[1] == "TimeRoom") {
          game$currentRoom$greet(
            game$currentRoom$floorMapsIdx %in% names(game$floorMapsPlayer))
          game$mode <- "time"
          game$trRiddleIdx <- 1
          game$riddle <- game$currentRoom$riddle[[game$trRiddleIdx]]
          if (!is.na(game$riddle$prepare)) {
            eval(parse(text = game$riddle$prepare))
          }
          message(paste0("問題 ", game$trRiddleIdx, " を ",
                         length(game$currentRoom$riddle), "で割ると:"))
          game$riddle$askQuestion()
        } else if (class(game$currentRoom)[1] == "DarkRoom") {
          game$mode <- "dark"
          game$currentRoom$greet(game$directionChosen)
          if (length(game$satchel) >= game$currentRoom$nObjectsLeave) {
            # subtract necessary R Power and go back to previous room
            message("正解!")
            game$removeNObjectsFromSatchel(game$currentRoom$nObjectsLeave)
            message("すばらしい! あなたは前の部屋に引き戻される。\n")
            game$directionChosen <-
              game$previousRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
            game$currentRoom <- game$previousRoom
            game$previousRoom <- NULL
            game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                             game$currentRoom$countLockedDoors() *
                                             game$lockedDoorDelay)
            game$currentRoom$greet(game$directionChosen)
            game$roomStartTime <- Sys.time()
            game$mode <- NULL
          } else {
            loseMessage <- paste0("おっと、 ", ifelse(length(game$satchel) == 0,
                                                    "あなたは",
                                                    paste0("を持っていないようだ。バックパックには ",
                                                           length(game$satchel))),
                                  " が入っている。")
            game$loseScenario(loseMessage)
            return(TRUE)
          }
        } else {
          game$currentRoom$greet(game$directionChosen)
        }
      } else if (game$mode == "object") {
        message(paste0("あなたは", game$currentRoom$object[[game$object_idx]]$name,
                       " を取り、バックパックにしまった。"))
        game$currentRoom$object[[game$object_idx]]$takeObject()
        game$satchel <- c(game$satchel, game$currentRoom$object[[game$object_idx]])
        game$satchelHist <- c(game$satchelHist, game$currentRoom$object[[game$object_idx]])
        objType <- game$currentRoom$object[[game$object_idx]]$type
        if (objType == "power") {
          message(paste0("あなたはR力を",
                         game$currentRoom$object[[game$object_idx]]$points,
                         " points ゲットした!"))
          game$RPower <- game$RPower + game$currentRoom$object[[game$object_idx]]$points
        } else if (objType == "pwd") {
          message(paste0("何か",
                         game$currentRoom$object[[game$object_idx]]$name,
                         "に書いてある。描画ウィンドウを見てみよう。"))
          nextPwdIdx <- ifelse(length(game$pwdExposedIdx) == length(game$pwd) - 1,
                               setdiff(1:length(game$pwd), game$pwdExposedIdx),
                               sample(setdiff(1:length(game$pwd),
                                              game$pwdExposedIdx), 1))
          game$pwdExposedIdx <- c(game$pwdExposedIdx, nextPwdIdx)
          game$plotPwd()
        } else if (objType == "tip") {
          message(paste0("何か ",
                         game$currentRoom$object[[game$object_idx]]$name,
                         "に描かれている。: ", game$currentRoom$object[[game$object_idx]]$riddle$tip))
        } else if (objType == "map") {
          newMapIdx <- game$currentRoom$object[[game$object_idx]]$riddle$floorMapsIdx
          mapsNames <- names(game$floorMapsPlayer)
          if (!newMapIdx %in% mapsNames) {
            game$floorMapsPlayer[[length(game$floorMapsPlayer) + 1]] <-
              game$floorMapsAvailable[[newMapIdx]]
            names(game$floorMapsPlayer) <- c(mapsNames, newMapIdx)
            message(paste0("地図のようだ!閲覧には seeMap(", newMapIdx, ")と入力しよう。"))
          } else {
            message("地図のようだが、 すでに持っている物のようだ。")
          }
        }
        game$object_idx <- NULL
        game$mode <- NULL
        game$riddle <- NULL
      } else if (game$mode == "time") {
        if (game$trRiddleIdx == length(game$currentRoom$riddle)) {
          # give player map if not already there
          if (!game$currentRoom$floorMapsIdx %in% names(game$floorMapsPlayer)) {
            mapsNames <- names(game$floorMapsPlayer)
            newMapIdx <- game$currentRoom$floorMapsIdx
            game$floorMapsPlayer[[length(game$floorMapsPlayer) + 1]] <-
              game$floorMapsAvailable[[newMapIdx]]
            names(game$floorMapsPlayer) <- c(mapsNames, newMapIdx)
            message(paste0("地図を手に入れた! 閲覧には seeMap(",
                           newMapIdx, ")と入力しよう。\n\n前の部屋に戻った。"))
          } else {
            message("地図のようだが、すでに持っているようだ...\n\n前の部屋に戻った。")
          }
          # return to previous room
          game$directionChosen <-
            game$previousRoom$door[[game$door_idx]]$getDirection(game$currentRoom$name)
          game$currentRoom <- game$previousRoom
          game$previousRoom <- NULL
          game$currentRoom$set_timeLimit(game$roomTimeLimit +
                                           game$currentRoom$countLockedDoors() *
                                           game$lockedDoorDelay)
          game$currentRoom$greet(game$directionChosen)
          game$roomStartTime <- Sys.time()
          game$mode <- NULL
          game$riddle <- NULL
        } else {
          game$trRiddleIdx <- game$trRiddleIdx + 1
          game$riddle <- game$currentRoom$riddle[[game$trRiddleIdx]]
          if (!is.na(game$riddle$prepare)) {
            eval(parse(text = game$riddle$prepare))
          }
          message(paste0("問題 ", game$trRiddleIdx, " を ",
                         length(game$currentRoom$riddle), "で割ると:"))
          game$riddle$askQuestion()
        }
      }
    }
  }
  
  # additional stuff
  currentRoom = lounge
  nextRoom = NULL
  previousRoom = NULL
  deparsedExpr = NULL
  directionChosen = NULL
  roomStartTime = NULL
  satchel = list()
  satchelHist = list()
  mode = NULL
  door_idx = NULL
  object_idx = NULL
  riddle = NULL
  RPower = 0
  trRiddleIdx = NULL
  floorMapsPlayer = list()
  pwdExposedIdx = NULL
  escapeRoom = osTower
  hintRPower = 1
  solutionRPower = 2
  dragonSeen = FALSE
  
  list2env(mget(ls()), envir = game)
  
  return(game)
}