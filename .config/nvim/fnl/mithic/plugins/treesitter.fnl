[{1 "nvim-treesitter/nvim-treesitter"
  :lazy false
  :build #((. (require :nvim-treesitter.install) :update) {:with_sync true})
  :main :nvim-treesitter.configs
  :opts
    {:ensure_installed [:lua :fennel :vim :vimdoc :query]
     :sync_install true
     :auto_install true
     :ignore_install [:verilog]
     :highlight
       {:enable true
        :additional_vim_regex_highlighting true
        :disable [:latex]}
     :indent {:enable true :disable {}}
     :incremental_selection
       {:enable true :disable []
        :keymaps {:init_selection "ga" :node_incremental "ga"
                  :node_decremental "gA" :scope_incremental false}}}}


 {1 "nvim-treesitter/nvim-treesitter-textobjects"
  :lazy false
  :priority 25
  :main :nvim-treesitter.configs
  :opts
    {:textobjects
       {:select
          {:enable true :lookahead true :include_surrounding_whitespace false
           :keymaps {"aa" "@parameter.outer" "ia" "@parameter.inner"
                     "af" "@function.outer" "if" "@function.inner"
                     "ac" "@class.outer" "ic" "@class.inner"}
           :selection_modes {"@parameter.outer" "v" "@parameter.inner" "v"
                             "@function.outer" "V" "@function.inner" "V"
                             "@class.outer" "V" "@class.inner" "V"}}
        :move {:enable true :set_jumps true
               :goto_next_end {"][" "@function.outer"}
               :goto_next_start {"]]" "@function.outer"}
               :goto_previous_end {"[]" "@function.outer"}
               :goto_previous_start {"[[" "@function.outer"}}
        :swap {:enable false}}}}


 {1 "nvim-treesitter/nvim-treesitter-context"
  :lazy false
  :priority 25
  :opts
    {:enable true
     :max_lines 4
     :min_window_height 24
     :line_numbers true
     :multiline_threshold 16
     :trim_scope :outer
     :mode :topline
     :separator "-"}}]
