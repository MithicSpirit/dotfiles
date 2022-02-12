autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif

call plug#begin()
Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
Plug 'itchyny/lightline.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'justinmk/vim-sneak'
Plug 'easymotion/vim-easymotion'
Plug 'lambdalisue/suda.vim'
Plug 'airblade/vim-gitgutter'
call plug#end()


set nobackup noswapfile lazyredraw
set mouse=a clipboard+=unnamedplus
set noshowmode ignorecase smartcase
set number cursorline 
set splitbelow splitright
set listchars=tab:»\ ,nbsp:␣,trail:·,lead:·,extends:$,precedes:$ list

set textwidth=80  " tw
set colorcolumn=+1  " cc
set tabstop=4  " ts
set softtabstop=4  " sts
set shiftwidth=4  " sw
set expandtab  " et/noet
augroup language_customizations
    autocmd FileType javascript,typescript,c,cpp,sh,zsh,fish,conf setlocal noet
    autocmd FileType haskell,lisp,tex,gitcommit setlocal ts=2 sts=2 sw=2
    autocmd FileType python setlocal tw=79
augroup END


nnoremap Y y$
nnoremap x "_x
nnoremap ~ g~l
nnoremap <silent> <ESC> :nohlsearch<CR>

nnoremap <C-w>, <C-w><
nnoremap <C-w>. <C-w>>

command W w
command Q q
command Wq wq

autocmd VimLeave * set guicursor=a:ver20

let g:tex_flavor = "latex"

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
