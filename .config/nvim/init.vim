set nobackup
set noswapfile
set t_Co=256
set number
set clipboard=unnamedplus
set smartcase
set splitbelow
set splitright
set mouse=a
set tabstop=4

nnoremap Y y$
nnoremap x "_x

nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>
nnoremap <C-h> <C-w><C-h>
nnoremap <C-,> <C-w><
nnoremap <C-.> <C-w>>

command W w
command Wsudo w !sudo tee % > /dev/null
