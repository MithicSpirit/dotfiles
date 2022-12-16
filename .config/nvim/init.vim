" Install plugins on first load
autocmd VimEnter *
    \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
    \|   PlugInstall --sync | q
    \| endif

" Set some configs
set nobackup noswapfile lazyredraw
set mouse=nvi clipboard+=unnamedplus
set noshowmode ignorecase smartcase
set number cursorline signcolumn=yes:1
set splitbelow splitright
set listchars=tab:»\ ,nbsp:␣,trail:·,lead:·,extends:$,precedes:$ list
set notimeout

let mapleader=""

set textwidth=80  " tw
set colorcolumn=+1,-6  " cc
set tabstop=8  " ts
set softtabstop=4  " sts
set shiftwidth=4  " sw
set expandtab  " et/noet
augroup language_customizations
    autocmd FileType
        \ javascript,typescript,json,c,cpp,sh,zsh,java
        \ setlocal ts=8 sts=8 sw=8 noet
    autocmd FileType
        \ haskell,lisp,tex,gitcommit,toml
        \ setlocal ts=8 sts=2 sw=2 et
    autocmd FileType python setlocal tw=79
augroup END

augroup detect_language
    autocmd BufRead,BufNewFile *.PKGBUILD set ft=sh
    autocmd BufRead,BufNewFile */xresources/* set ft=xdefaults
augroup END

" Load plugins
call plug#begin()
    Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
    Plug 'itchyny/lightline.vim'
    Plug 'arcticicestudio/nord-vim'
    Plug 'justinmk/vim-sneak'
    Plug 'easymotion/vim-easymotion'
    Plug 'lambdalisue/suda.vim'
    Plug 'airblade/vim-gitgutter'
    Plug 'whonore/Coqtail'
    Plug 'editorconfig/editorconfig-vim'
    Plug 'edwinb/idris2-vim'
call plug#end()

" Bind keys
nnoremap Y y$
nnoremap x "_x
nnoremap ~ g~l
nnoremap <silent> <ESC> :nohlsearch<CR>

nnoremap <C-w>, <C-w><
nnoremap <C-w>. <C-w>>
nnoremap <silent> <C-w>t :split +terminal<CR>

command W w
command Q q
command Wq wq

" Miscellaneous
"autocmd VimLeave * set guicursor=a:ver20
let g:tex_flavor = "latex"
let g:rust_recommended_style = 0

" Terminal
augroup terminal_options
    autocmd TermOpen * set nonumber
    autocmd TermOpen * startinsert
augroup END


" Firenvim
let g:firenvim_config = { 
    \ 'localSettings': {
        \ '.*': {
            \ 'takeover': 'never',
        \ },
    \ }
\ }

" Lightline
source ~/.config/nvim/lightline-conf.vim

" Nord theme
let g:nord_cursor_line_number_background = 1
let g:nord_italic_comments = 1
colorscheme nord

" Vim sneak
map f <Plug>Sneak_f
map F <Plug>Sneak_F
map t <Plug>Sneak_t
map T <Plug>Sneak_T

" Vim easymotion
map gs <Plug>(easymotion-prefix)

" Suda.vim
command Wsudo w suda://%

" Vim gitgutter
set updatetime=500
"let g:gitgutter_terminal_reports_focus=0
