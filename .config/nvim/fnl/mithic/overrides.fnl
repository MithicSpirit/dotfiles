(local augroup (vim.api.nvim_create_augroup :mithic-overrides {}))


;; Options
(vim.api.nvim_create_autocmd :FileType
  {:pattern [:c :cpp :java :javascript :typescript :json :zig]
   :callback #(do
               (set vim.opt_local.shiftwidth 8)
               (set vim.opt_local.expandtab false)
               (set vim.opt_local.cindent true))
   :group augroup})

(vim.api.nvim_create_autocmd :FileType
  {:pattern [:sh :zsh :asm :lua :conf :rust :zig :tsv]
   :callback #(do
               (set vim.opt_local.shiftwidth 8)
               (set vim.opt_local.expandtab false))
   :group augroup})

(vim.api.nvim_create_autocmd :FileType
  {:pattern [:haskell :lisp :tex :gitcommit :toml :markdown :lean :lean3
             :fennel :idris :idris2 :agda]
   :callback #(do
               (set vim.opt_local.shiftwidth 2)
               (set vim.opt_local.expandtab true))
   :group augroup})

(vim.api.nvim_create_autocmd :FileType
  {:pattern :python
   :callback #(set vim.opt_local.textwidth 79)
   :group augroup})

(vim.api.nvim_create_autocmd :FileType
  {:pattern [:lean :lean3]
   :callback #(set vim.opt_local.textwidth 100)
   :group augroup})

(vim.api.nvim_create_autocmd :FileType
  {:pattern :help
   :callback #(do
               (set vim.opt_local.scrolloff 1)
               (set vim.opt_local.colorcolumn nil))
   :group augroup})


;; Mappings
(vim.api.nvim_create_autocmd :FileType
 {:pattern :tex
  ; TODO: use a snippet
  :callback
    #(do (vim.keymap.set :i "\"" "``" {:buffer true})
         (vim.keymap.set :i "$" "\\(" {:buffer true}))
  :group augroup})

(vim.api.nvim_create_autocmd :LspAttach
 {:callback
    #(let [opts {:buffer true}
           (tb-ok tb) (pcall #(require :telescope.builtin))]
        (each [k v (pairs
                     {"gd" vim.lsp.buf.definition
                      "gI" vim.lsp.buf.implementation
                      "K" vim.lsp.buf.hover
                      "gD" vim.lsp.buf.type_definition
                      "gQ" vim.lsp.buf.format
                      "<leader>la" vim.lsp.buf.code_action
                      "<leader>lc" vim.lsp.codelens.run
                      "<leader>lr" vim.lsp.buf.rename})]
          (vim.keymap.set :n k v opts))
        (when tb-ok
          (each [k v (pairs
                       {"gR" tb.lsp_references
                        "<leader>lsd" tb.lsp_document_symbols
                        "<leader>lsw" tb.lsp_dynamic_workspace_symbols})]
            (vim.keymap.set :n k v opts))))
  :group augroup})


;; Filetype identification
(vim.api.nvim_create_autocmd [:BufRead :BufNewFile]
 {:pattern "*PKGBUILD"
  :callback #(vim.cmd.setf :PKGBUILD)
  :group augroup})

(vim.api.nvim_create_autocmd [:BufRead :BufNewFile]
 {:pattern "*SRCINFO"
  :callback #(vim.cmd.setf :SRCINFO)
  :group augroup})

(vim.api.nvim_create_autocmd [:BufRead :BufNewFile]
 {:pattern :*/xresources/*
  :callback #(vim.cmd.setf :xdefaults)
  :group augroup})


;; Misc
(vim.api.nvim_create_autocmd :TextYankPost
  {:callback #(vim.highlight.on_yank) :group augroup})

(vim.api.nvim_create_autocmd :TermOpen
 {:callback #(do (set vim.opt_local.number false)
                 (set vim.opt_local.relativenumber false))
  :group augroup})
