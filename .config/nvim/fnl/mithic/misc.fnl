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
  :group (vim.api.nvim_create_augroup :mithic-misc {})})


;; Neovide
(when vim.g.neovide
  (set vim.g.neovide_transparency 0.93)
  (set vim.g.neovide_hide_mouse_when_typing false)
  (set vim.g.neovide_cursor_animation_length 0.02)
  (set vim.g.neovide_cursor_trail_size 0.5)
  (set vim.g.neovide_cursor_animate_in_insert_mode true)
  (set vim.g.neovide_cursor_animate_switch_to_command_line true)
  (set vim.g.neovide_cursor_unfocued_outline_width 0.11))
