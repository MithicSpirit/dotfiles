;; Options
(set vim.opt.backup false)
(set vim.opt.swapfile false)

(set vim.opt.showmode false)
(set vim.opt.termguicolors false)
(set vim.opt.mouse :nv)
(set vim.opt.conceallevel 2)
(set vim.opt.guifont "Iosevka Mithic:h12:#e-antialias:#h-full")
(set vim.opt.linespace 0)
(set vim.opt.timeout false)
(set vim.opt.cursorline true)
(set vim.opt.allowrevins true)
(set vim.opt.tildeop true)
(vim.opt.path:append "**")

(set vim.opt.spelllang :en_us)
(set vim.opt.spellfile (.. (vim.fn.stdpath :data) "/spellfile.utf-8.add"))

(set vim.opt.ignorecase true)
(set vim.opt.smartcase true)
(set vim.opt.hlsearch true)
(set vim.opt.incsearch true)

(set vim.opt.undodir (.. (vim.fn.stdpath :cache) "/undodir"))
(set vim.opt.undofile true)
(set vim.opt.undolevels (^ 2 15))

(set vim.opt.scrolloff 6)
(set vim.opt.sidescrolloff 6)
(set vim.opt.number true)
(set vim.opt.relativenumber false)
(set vim.opt.numberwidth 4)
(set vim.opt.signcolumn :yes:1)

(set vim.opt.splitbelow true)
(set vim.opt.splitright true)

(set vim.opt.textwidth 80)
(set vim.opt.colorcolumn "+1")
(set vim.opt.wrap false)

(set vim.opt.tabstop 8)
(set vim.opt.shiftwidth 4)
(set vim.opt.softtabstop -1)  ; stay in sync with sw
(set vim.opt.expandtab true)

(set vim.g.loaded_node_provider 0)
(set vim.g.loaded_perl_provider 0)
(set vim.g.loaded_ruby_provider 0)

(set vim.g.tex_flavor :latex)
(set vim.g.zig_fmt_autosave 0)

(set vim.g.markdown_recommended_style 0)
(set vim.g.python_recommended_style 0)
(set vim.g.rust_recommended_style 0)

(set _G.border "rounded")

(set vim.opt.listchars
   {:extends "$" :lead "|" :nbsp "~" :precedes "$" :tab "> " :trail "⏹"})
(set vim.opt.list true)
(fn update-listchars-leadmultispace []
  (vim.opt_local.listchars:append
    {:leadmultispace (.. "|" (string.rep " " (- vim.bo.shiftwidth 1)))})
  (vim.opt_global.listchars:append
    {:leadmultispace (.. "|" (string.rep " " (- vim.go.shiftwidth 1)))}))
(update-listchars-leadmultispace)
(vim.api.nvim_create_autocmd [:OptionSet :BufWinEnter]
  {:callback update-listchars-leadmultispace
   :group (vim.api.nvim_create_augroup :mithic-lc-lms {})})

;; Keybinds
(set vim.g.mapleader " ")
(set vim.g.maplocalleader "\\")
(vim.keymap.set "" "<leader>" "<Nop>")
(vim.keymap.set "" "<localleader>" "<Nop>")

(vim.keymap.set :n "Y" :y$)
(vim.keymap.set :n "x" "\"_x")
(vim.keymap.set :n "~~" "g~l")
(vim.keymap.set :n "<Esc>" #(do (vim.cmd.nohlsearch) (vim.cmd.mode)))
(vim.keymap.set :n "ZQ" vim.cmd.cquit)

(vim.keymap.set :n "<leader>s" vim.cmd.write)
(vim.keymap.set :n "<leader>E"
  #(do (vim.cmd.write) (vim.cmd "13split +terminal\\ %:p")))
(vim.keymap.set :n "<leader>t" #(do (vim.cmd "19split +terminal")
                                    (vim.cmd.startinsert)))

(vim.keymap.set [:n :v] "<leader>y" "\"+y" {:remap true})
(vim.keymap.set [:n :v] "<leader>Y" "\"+Y" {:remap true})
(vim.keymap.set :n "<leader><C-Y>" #(vim.fn.setreg "+" (vim.fn.getreg "")))
(vim.keymap.set [:n :v] "<leader>p" "\"+p" {:remap true})
(vim.keymap.set [:n :v] "<leader>P" "\"+P" {:remap true})

(vim.keymap.set :n "gQ"
  #(let [c (vim.api.nvim_win_get_cursor 0)]
     (vim.cmd "normal! gggqG")
     (pcall #(vim.api.nvim_win_set_cursor 0 c))))
(vim.keymap.set :n "<C-W>," "<C-w><")
(vim.keymap.set :n "<C-W>." "<C-w>>")
(vim.keymap.set :t "<C-\\><C-\\>" "<C-\\><C-n>")
(vim.keymap.set :v "J" ":move '>+1<CR>gv" {:silent true})
(vim.keymap.set :v "K" ":move '<-2<CR>gv" {:silent true})
(vim.keymap.set :v ">" ">gv")
(vim.keymap.set :v "<" "<gv")
(vim.keymap.set :n "<C-d>" "<C-d>zz")
(vim.keymap.set :n "<C-u>" "<C-u>zz")
(vim.keymap.set :n "n" "nzz")
(vim.keymap.set :n "N" "Nzz")

(vim.keymap.set :i "<C-Bs>" "<C-h>" {:remap true})
(vim.keymap.set :i "<C-h>" "<C-g>u<C-w><C-g>u")

(vim.keymap.set :n "<leader>ld" vim.diagnostic.open_float)
(vim.keymap.set "" "[d" vim.diagnostic.goto_prev)
(vim.keymap.set "" "]d" vim.diagnostic.goto_next)

(vim.keymap.set "" "s" "<Nop>")
(vim.keymap.set "" "S" "<Nop>")

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

(vim.api.nvim_create_user_command :Cdfile
  #(vim.api.nvim_set_current_dir (vim.fn.expand "%:p:h"))
  {:desc "cd to the parent of the current file" :force false})


(require (.. ... :.overrides))
(require (.. ... :.misc))
