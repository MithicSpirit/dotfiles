(local logo
  [" ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗"
   " ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║"
   " ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║"
   " ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║"
   " ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║"
   " ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝"])

(fn afterlogo []
  (let [(lz-ok lz) (pcall #(require :lazy.manage.checker))]
    [(when lz-ok (.. "Lazy updates: " (length lz.updated)))]))


(fn runtime-cond []
  (and (= (vim.api.nvim_buf_line_count 0) 1)
       (= (. (vim.api.nvim_buf_get_lines 0 0 -1 false) 1) "")
       (= (vim.api.nvim_buf_get_name 0) "")
       (= (vim.api.nvim_get_option_value :filetype {}) "")))


(fn display []
  (local buf (vim.api.nvim_create_buf "nobuflisted" "unlisted"))

  (vim.api.nvim_buf_set_name buf "intro")
  (vim.api.nvim_set_option_value :bufhidden :wipe {:buf buf})
  (vim.api.nvim_set_option_value :buftype :nofile {:buf buf})
  (vim.api.nvim_set_option_value :filetype :mithic-intro {:buf buf})
  (vim.api.nvim_set_option_value :swapfile false {:buf buf})

  (var display [])
  (each [_ l (ipairs logo)]
    (table.insert display l))
  (table.insert display "")
  (each [_ l (ipairs (afterlogo))]
    (when l (table.insert display l)))

  (let [start_row
         (math.floor (/ (- (vim.api.nvim_win_get_height
                             (vim.api.nvim_get_current_win))
                           vim.o.cmdheight
                           (length display))
                        2))]
    (when (> start_row 0) (for [_ 1 start_row] (table.insert display 1 ""))))

  (vim.api.nvim_buf_set_lines buf 0 -1 false display)


  (let [cur (vim.api.nvim_get_current_buf)]
    (vim.api.nvim_set_current_buf buf)
    (vim.api.nvim_buf_delete cur {}))

  (vim.cmd (.. "%center"
               (vim.api.nvim_win_get_width (vim.api.nvim_get_current_win))))
  (vim.api.nvim_set_option_value :number false {:buf buf})
  (vim.api.nvim_set_option_value :list false {:buf buf})
  (vim.api.nvim_set_option_value :colorcolumn "" {:buf buf})
  (vim.api.nvim_set_option_value :textwidth 0 {:buf buf})
  (vim.api.nvim_set_option_value :modifiable false {:buf buf}))


(when (and (= (vim.fn.argc) 0) (not (string.find vim.o.shortmess "I")))
  (vim.api.nvim_create_autocmd :UIEnter
   {:callback #(when (runtime-cond) (display))
    :group (vim.api.nvim_create_augroup :mithic-intro {})
    :once true})
  (vim.opt.shortmess:append "I"))
