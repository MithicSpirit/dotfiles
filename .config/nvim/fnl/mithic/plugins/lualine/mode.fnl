(let [modemap
       {"!"      "SHELL "
        "\019"   "S-BLCK"
        "\022"   "V-BLCK"
        "\022s"  "V-BLCK"
        "c"      "COMMND"
        "cv"     "EX-CMD"
        "i"      "INSERT"
        "ic"     "INSERT"
        "ix"     "INSERT"
        "n"      "NORMAL"
        "niI"    "NORMAL"
        "niR"    "NORMAL"
        "niV"    "NORMAL"
        "no"     "PENDNG"
        "noV"    "PENDNG"
        "no\022" "PENDNG"
        "nov"    "PENDNG"
        "nt"     "NORMAL"
        "ntT"    "NORMAL"
        "r"      "PROMPT"
        "r?"     "CONFRM"
        "rm"     "MORE  "
        "R"      "REPLCE"
        "Rc"     "REPLCE"
        "Rv"     "V-RPLC"
        "Rvc"    "V-RPLC"
        "Rvx"    "V-RPLC"
        "Rx"     "REPLCE"
        "s"      "SELECT"
        "S"      "S-LINE"
        "t"      "TERMNL"
        "v"      "VISUAL"
        "vs"     "VISUAL"
        "V"      "V-LINE"
        "Vs"     "V-LINE"}]
  #(let [mode-code (. (vim.api.nvim_get_mode) :mode)]
    (or (. modemap mode-code) mode-code)))
