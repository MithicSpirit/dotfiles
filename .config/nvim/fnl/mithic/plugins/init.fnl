[{1 "tpope/vim-sleuth"
  :lazy false}

 {1 "tpope/vim-eunuch"
  :lazy false}

 {1 "tpope/vim-rsi"
  :lazy false}

 {1 "tpope/vim-surround"
  :dependencies ["tpope/vim-repeat"]
  :init #(set vim.g.surround_no_insert_mappings 0)
  :keys [ "ds" "cs" "cS" "ys" "yS" "yss" "ySs" "ySS"
         {1 "S" :mode :x}
         {1 "gS" :mode :x}
         {1 "<C-S>" 2 "<Plug>Isurround" :mode :i}]}

 {1 "tpope/vim-characterize"
  :keys [["gA" "<Plug>(characterize)"]]}

 {1 "tpope/vim-commentary"
  :keys [{1 "gc" :mode [:x :n :o]}]
  :cmd [:Commentary]}


 {1 "justinmk/vim-sneak"
   :init #(tset vim.g :sneak#use_ic_scs 1)
   :keys [["f" "<Plug>Sneak_f"]
          ["F" "<Plug>Sneaf_F"]
          ["t" "<Plug>Sneak_t"]
          ["T" "<Plug>Sneak_T"]]}


 {1 "mbbill/undotree"
  :keys [["<leader>u" vim.cmd.UndotreeToggle]]}

 {1 "chrisbra/Colorizer"
  :cmd [:ColorHighlight :ColorClear :RGB2Term :HSL2RGB :Term2RGB :ColorContrast
        :ColorSwapFgBg :ColorToggle]}


 {1 "gpanders/nvim-parinfer"
  :ft [:clojure :scheme :lisp :racket :hy :fennel :janet :carp :wast :yuck]}

 {1 "whonore/Coqtail"
  :init #(set vim.g.coqtail_map_prefix "<localleader>")
  :event ["BufReadPre *.v" "BufNewFile *.v"]
  :ft :coq}

 {1 "edwinb/idris2-vim"
  :event ["BufReadPre *.idr" "BufNewFile *.idr"
          "BufReadPre *.lidr" "BufNewFile *.lidr"]
  :ft [:idris2 :lidris2]}]
