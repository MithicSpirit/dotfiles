;; Netrw
(set vim.g.netrw_liststyle 3)
(set vim.g.netrw_banner 0)
(set vim.g.netrw_preview 1)
(vim.keymap.set :n "<leader>." #(vim.cmd.edit "."))
(vim.api.nvim_create_autocmd :FileType
 {:pattern :netrw
  :callback
    #(let [opts {:buffer 0 :remap true}]
       (vim.keymap.set :n "l" "<Cr>" opts)
       (vim.keymap.set :n "M" "mfj" opts)
       (vim.keymap.set :n "i" "R" opts)
       (vim.keymap.set :n "o" "%" opts))
  :group (vim.api.nvim_create_augroup :mithic-netrw {})})


;; Neovide
(when vim.g.neovide
  (set vim.g.neovide_transparency 0.93)
  (set vim.g.neovide_hide_mouse_when_typing false)
  (set vim.g.neovide_cursor_animation_length 0.02)
  (set vim.g.neovide_cursor_trail_size 0.5)
  (set vim.g.neovide_cursor_animate_in_insert_mode true)
  (set vim.g.neovide_cursor_animate_switch_to_command_line true)
  (set vim.g.neovide_cursor_unfocued_outline_width 0.11))


;; Popup border
(vim.diagnostic.config {:float {:border _G.border}})
(each [name handler (pairs vim.lsp.handlers)]
  (tset vim.lsp.handlers name (vim.lsp.with handler {:border _G.border})))


;; Adjust lc-lms by sw
(fn update-listchars-leadmultispace []
  (vim.opt_local.listchars:append
    {:leadmultispace (.. "|" (string.rep " " (- vim.bo.shiftwidth 1)))})
  (vim.opt_global.listchars:append
    {:leadmultispace (.. "|" (string.rep " " (- vim.go.shiftwidth 1)))}))
(update-listchars-leadmultispace)
(vim.api.nvim_create_autocmd [:OptionSet :BufWinEnter]
  {:callback update-listchars-leadmultispace
   :group (vim.api.nvim_create_augroup :mithic-lc-lms {})})


;; Whitespace cleanup
(vim.api.nvim_create_autocmd :BufWritePre
  {:callback
   #(let [c (vim.api.nvim_win_get_cursor 0)]
      (vim.cmd "%substitute/\\s\\+$//e")
      (vim.cmd "%substitute/\\n\\+\\%$//e")
      (pcall #(vim.api.nvim_win_set_cursor 0 c))
      nil)
   :group (vim.api.nvim_create_augroup :mithic-whitespace {})})


;; Jump to last position
(vim.api.nvim_create_autocmd :BufRead
  {:callback
     #(vim.api.nvim_create_autocmd :FileType
        {:callback
           #(when (and (~= vim.o.ft :gitcommit)
                       (~= vim.o.ft :gitrebase)
                       (<= 0 (vim.fn.line "'\"") (vim.fn.line "$"))
                       (= 1 (vim.fn.line ".")))
              (vim.cmd "normal! g'\""))
         :buffer 0
         :once true})
   :group (vim.api.nvim_create_augroup :mithic-lastpos {})})


;; New scratch buffer
(vim.keymap.set :n "<leader>b"
  #(do
     (fn get-scratch [buffers index]
       (if (< index 1) (let [buf (vim.api.nvim_create_buf false false)]
                         (vim.api.nvim_buf_set_option buf :buftype :nofile)
                         (vim.api.nvim_buf_set_option buf :swapfile false)
                         (vim.api.nvim_buf_set_option buf :filetype :text)
                         (vim.api.nvim_buf_set_name buf "/scratch")
                         buf)
           (= (vim.api.nvim_buf_get_name (. buffers index)) "/scratch") index
           (get-scratch buffers (- index 1))))

     (let [bufs (vim.api.nvim_list_bufs)]
       (vim.api.nvim_win_set_buf 0 (get-scratch bufs (# bufs))))))


;; Buffer movement keybinds
(vim.keymap.set :n "<Tab>" "<C-^>")
(vim.keymap.set :n "<S-Tab>"
  #(if (vim.opt_local.modified:get)
       (vim.api.nvim_err_writeln "E89: No write since last change for buffer")
     (let [curr-buf (vim.api.nvim_get_current_buf)]
       (var last-2buf nil)
       (var last-buf nil)
       (each [_ buf (ipairs (vim.api.nvim_list_bufs))
                &until (= last-2buf curr-buf)]
         (set last-2buf last-buf) (set last-buf buf))
       (when last-2buf (vim.api.nvim_set_current_buf last-buf))
       (vim.cmd (.. "bdelete " curr-buf)))))
(vim.keymap.set :n "<leader><Tab>" vim.cmd.bnext)
(vim.keymap.set :n "<leader><S-Tab>" vim.cmd.bprevious)
