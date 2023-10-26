(fn minisetup [mod opts]
  ((. (require (.. :mini. mod)) :setup) (or opts {})))

[{1 "echasnovski/mini.nvim"
  :lazy false
  :enabled false
  :config #(do
            (minisetup :files
               {:content {:filter nil :prefix nil :sort nil}
                :mappings
                  {:close "q"
                   :go_in "l"
                   :go_in_plus "L"
                   :go_out "h"
                   :go_out_plus "H"
                   :reset "<BS>"
                   :reveal_cwd "@"
                   :show_help "g?"
                   :synchronize "="
                   :trim_left "<"
                   :trim_right ">"}
                :options {:permanent_delete true :use_as_default_explorer true}
                :windows
                  {:max_number math.huge
                   :preview true
                   :width_focus 50
                   :width_nofocus 15
                   :width_preview 25}}))}]
