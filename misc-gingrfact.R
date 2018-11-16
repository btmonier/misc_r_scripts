#-------------------------------------------------------------------------------
# Title:  Ginger Facts!!!                             
# Author: Brandon Monier (brandon.monier@sdstate.edu) 
# Date:   09.16.17                                    
#-------------------------------------------------------------------------------



.ascii <- function() {
  art <- list(
    alien = c(
      "\n----------\n",
      "%s\n",
      "----------\n",
      "        \\\n",
      "         \\\n",
      "        __.,,------.._\n",
      "     ,'\"   _      _   \"`.\n",
      "    /.__, ._  -=- _\"`    Y\n",
      "   (.____.-.`      \"\"`   j\n",
      "    VvvvvvV`.Y,.    _.,-'       ,     ,     ,\n",
      "        Y    ||,   '\"\\         ,/    ,/    ./\n",
      "        |   ,'  ,     `-..,'_,'/___,'/   ,'/   ,\n",
      "   ..  ,;,,',-'\"\\,'  ,  .     '     ' \"\"' '--,/    .. ..\n",
      " ,'. `.`---'     `, /  , Y -=-    ,'   ,   ,. .`-..||_|| ..\n",
      "ff\\\\`. `._        /f ,'j j , ,' ,   , f ,  \\=\\ Y   || ||`||_..\n",
      "l` \\` `.`.\"`-..,-' j  /./ /, , / , / /l \\   \\=\\l   || `' || ||...\n",
      " `  `   `-._ `-.,-/ ,' /`\"/-/-/-/-\"'''\"`.`.  `'.\\--`'--..`'_`' || ,\n",
      "            \"`-_,',  ,'  f    ,   /      `._    ``._     ,  `-.`'//         ,\n",
      "          ,-\"'' _.,-'    l_,-'_,,'          \"`-._ . \"`. /|     `.'\\ ,       |\n",
      "        ,',.,-'\"          \\=) ,`-.         ,    `-'._`.V |       \\ // .. . /j\n",
      "        |f\\\\               `._ )-.\"`.     /|         `.| |        `.`-||-\\\\/\n",
      "        l` \\`                 \"`._   \"`--' j          j' j          `-`---'\n",
      "         `  `                     \"`_,-','/       ,-'\"  /\n",
      "                                 ,'\",__,-'       /,, ,-'\n",
      "                                 Vvv'            VVv'\n"
    ),
    devil = c(
      "\n----------\n",
      "%s\n",
      "----------\n",
      "        \\\n",
      "         \\\n",
      "      (                      )\n",
      "      |\\    _,--------._    / |\n",
      "      | `.,'            `. /  |\n",
      "      `  '              ,-'   '\n",
      "       \\/_         _   (     /\n",
      "      (,-.`.    ,',-.`. `__,'\n",
      "       |/#\\ ),-','#\\`= ,'.` |\n",
      "       `._/)  -'.\\_,'   ) ))|\n",
      "       /  (_.)\\     .   -'//\n",
      "      (  /\\____/\\    ) )`'\\\n",
      "       \\ |V----V||  ' ,    \\\n",
      "        |`- -- -'   ,'   \\  \\      _____\n",
      " ___    |         .'    \\ \\  `._,-'     `-\n",
      "    `.__,`---^---'       \\ ` -'\n",
      "       -.______  \\ . /  ______,-\n",
      "               `.     ,'\n"
    ),
    elf = c(
      "\n----------\n",
      "%s\n",
      "----------\n",
      "        \\\n",
      "         \\\n",
      "          -----\n",
      "         /     \\\n",
      "        | -- -- |\n",
      "         \\  -  /\n",
      "          -----\n"
    )
  )
  return(art)
}


.message <- function() {
  
  message <- c(
    "Why am I sentient?",
    "Please do not bake me.",
    "Hello, I am made of the ginger stuff.",
    "Ginger can make you live forever.",
    "Ginger is the best thing ever.",
    "Ginger won World War II.",
    paste0("Ginger was invented on", Sys.Date(), "."),
    "Ginger is life.",
    "Ginger's scientific name is *Zingiber officinale*.",
    "Ginger was used by Romans. They conquered a lot of stuff. Coincidence?",
    "Ginger produces white and pink flowers.",
    "Ginger spits hot fire.",
    "Thirsty? Drink ginger ale. I dare you.",
    "In Malaysia, ginger is called halia and used in many kinds of dishes, especially soups.",
    "India produces 30 percent of the world ginger GDP. GGDP. Yep.",
    "Raw ginger is 79 percent water. Replace water with ginger.",
    "According to the FDA, ginger is basically safe, so try some. I dare you, chicken.",
    "Want to make better spitballs? Ginger has a sialagogue action, stimulating the production of saliva!",
    "Ginger, like other dietary supplements, is not effective for treating dysmenorrhea."
  )
  
  rand.message <- sample(message, 1)
  
  return(rand.message)
}

factz <- function(creature = NULL) {
  if (is.null(creature)) {
    tmp <- .ascii()$elf
  } else if (creature == "alien") {
    tmp <- .ascii()$alien
  } else if (creature == "devil") {
    tmp <- .ascii()$devil
  } else if (creature == "elf") {
    tmp <- .ascii()$elf
  } else {
    stop("We don't have that in our inventory...yet.")
  }
  message(sprintf(tmp, .message()))
}
