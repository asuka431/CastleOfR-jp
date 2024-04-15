#' Start the Castle of R game
#'
#' This function will start the Castle of R game. If you played the game before,
#' ended the game in the middle and chose to save your progress - this
#' function will identify this and will give you the option to pick up where
#' you left. It will also suggest to clean your workspace and plots entirely,
#' for a better game experience. Be careful before accepting this suggestion.
#' 
#' @examples
#' startGame()
#' 
#' @export
startGame <- function(...){
  if (exists(".gameOn")) {
    message("Game is already on, you can endGame() and come back later.")
    invisible()
  } else {
    removeTaskCallback("CastleOfR")
    message("Before you start, can I clean your workspace and plots?")
    cleanAns <- menu(c("yes", "no")) == 1
    if (cleanAns) {
      graphics.off()
      tryCatch(rm(list = ls(envir = globalenv(), all.names = TRUE), envir = globalenv()),
               warning = function(w) {invisible()})
    }
    .gameOn <<- TRUE
    continue <- FALSE
    playerLevel <- NULL
    if (file.exists(file.path(find.package("CastleOfR"), "CastleOfR_game.RData"))) {
      message("You've been here before. Pick up from where you left?")
      continue <- menu(c("yes", "no")) == 1
    }
    game <- if (continue) {
      readRDS(file.path(find.package("CastleOfR"), "CastleOfR_game.RData"))
    } else {
      message("あなたはRの城に居るRﾁｬﾝに、お茶会へ来ませんか、と招待を受け取った。")
      message("貴方のR言語に対する習熟度は?")
      playerLevel <- menu(c("あぁ、なんでもできる程度です",
                            "かなり使いこなせます",
                            "ちょっと慣れてきた",
                            "Rって何ですか"))
      initializeGame(ifelse(playerLevel == 0, 4, playerLevel))
    }
    cb <- function(expr, val, ok, vis, data = game){
      game$expr <- expr
      game$val <- val
      game$ok <- ok
      game$vis <- vis
      return(react(game, ...))
    }
    if (continue && game$currentRoom$name != "lounge") {
      game$currentRoom$greet()
    } else {
      game$currentRoom$startScenario()
    }
    game$roomStartTime <- Sys.time()
    addTaskCallback(cb, name = "CastleOfR")
    invisible()
  }
}