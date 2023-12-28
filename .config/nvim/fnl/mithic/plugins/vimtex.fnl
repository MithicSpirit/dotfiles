[{1 "lervag/vimtex"
  :ft [:tex]
  :event ["BufReadPre *.cls,*.tikz" "BufNewFile *.cls,*.tikz"]
  :cmd [:VimtexInverseSearch]
  :config #(do
            (set vim.g.vimtex_enabled 1)
            (set vim.g.vimtex_compiler_method :latexmk)
            (set vim.g.vimtex_mappings_prefix "<localleader>")
            (set vim.g.vimtex_text_obj_enabled 1)

            (set vim.g.vimtex_complete_enabled 1)
            (set vim.g.vimtex_complete_close_braces 1)

            (set vim.g.vimtex_indent_enabled 1)
            (set vim.g.vimtex_indent_bib_enabled 1)
            (set vim.g.vimtex_indent_tikz_commands 1)
            (set vim.g.vimtex_indent_on_ampersands 0)
            (set vim.g.vimtex_indent_delims {:close_indented 1})

            (set vim.g.vimtex_syntax_enabled 1)
            (set vim.g.vimtex_syntax_conceal
               {:accents 1
                :cites 1
                :fancy 1
                :greek 1
                :ligatures 1
                :math_bounds 1
                :math_delimiters 1
                :math_fracs 0
                :math_super_sub 1
                :math_symbols 1
                :sections 0
                :spacing 0
                :styles 1})

            (set vim.g.vimtex_syntax_conceal_cites {:type :brackets :verbose true})

            (set vim.g.vimtex_view_enabled 1)
            (set vim.g.vimtex_view_automatic 1)
            (set vim.g.vimtex_view_method :zathura_simple)

            (set vim.g.vimtex_format_enabled 1))}]
