set nobackup
set noswapfile
set number
set clipboard+=unnamedplus
set ignorecase
set smartcase
set mouse=a
set lazyredraw
set noshowmode
set cursorline

set splitbelow
set splitright

set tabstop=4
set softtabstop=4
set shiftwidth=4

nnoremap Y y$
nnoremap x "_x
nnoremap ~ g~l
nnoremap <silent> <ESC> :nohlsearch<CR>

nnoremap <C-w>, <C-w><
nnoremap <C-w>. <C-w>>

command W w
command Q q
command Wq wq
command Wsudo w !sudo tee % >/dev/null

autocmd VimLeave * set guicursor=a:ver20

call plug#begin()
Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
Plug 'itchyny/lightline.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'justinmk/vim-sneak'
Plug 'easymotion/vim-easymotion'
call plug#end()

let g:firenvim_config = { 
    \ 'localSettings': {
        \ '.*': {
            \ 'takeover': 'never',
        \ },
    \ }
\ }

source ~/.config/nvim/lightline-conf.vim

let g:nord_cursor_line_number_background = 1
let g:nord_italic_comments = 1
colorscheme nord

map f <Plug>Sneak_f
map F <Plug>Sneak_F
map t <Plug>Sneak_t
map T <Plug>Sneak_T

map gs <Plug>(easymotion-prefix)
