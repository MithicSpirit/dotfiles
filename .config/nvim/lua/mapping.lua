vim.g.mapleader = ' '
vim.g.maplocalleader = '\\'

vim.keymap.set('n', '<leader>', '<Nop>')
vim.keymap.set('n', '<localleader>', '<Nop>')

vim.keymap.set('n', 'Y', 'y$')
vim.keymap.set('n', 'x', '"_x')
vim.keymap.set('n', '~', 'g~l')
vim.keymap.set('n', '<ESC>', function()
	vim.cmd.nohlsearch()
	vim.cmd.diffupdate()
	vim.cmd.mode()
end)
vim.keymap.set('n', 'gQ', "mzgggqG'z")
vim.keymap.set('n', '<leader>s', vim.cmd.write)

vim.keymap.set('n', '<C-W>,', '<C-w><')
vim.keymap.set('n', '<C-W>.', '<C-w>>')
vim.keymap.set('n', '<leader>t', function() vim.cmd('19split +terminal') end)
vim.keymap.set('t', '<C-\\><C-\\>', '<C-\\><C-n>')

vim.keymap.set('v', 'J', ":move '>+1<CR>gv")
vim.keymap.set('v', 'K', ":move '<-2<CR>gv")

vim.keymap.set('v', '>', '>gv')
vim.keymap.set('v', '<', '<gv')

vim.keymap.set('n', '<C-d>', '<C-d>zz')
vim.keymap.set('n', '<C-u>', '<C-u>zz')
vim.keymap.set('n', 'n', 'nzvzz')
vim.keymap.set('n', 'N', 'Nzvzz')

vim.keymap.set('n', '<Tab>', vim.cmd.bnext)
vim.keymap.set('n', '<S-Tab>', vim.cmd.bprevious)
vim.keymap.set('n', '<leader><Tab>', '<C-^>')
vim.keymap.set('n', '<leader><S-Tab>', function()
	vim.cmd([[
let btarget = bufnr('%')
if btarget < 0
    call s:Warn('No buffer to delete')
else
    " Numbers of windows that view target buffer which we will delete.
    let wnums = filter(range(1, winnr('$')), 'winbufnr(v:val) == btarget')
    let wcurrent = winnr()
    for w in wnums
        execute w.'wincmd w'
        let prevbuf = bufnr('#')
        if prevbuf > 0 && buflisted(prevbuf) && prevbuf != btarget
            buffer #
        else
            bprevious
        endif
        if btarget == bufnr('%')
            " Numbers of listed buffers which are not the target to be deleted.
            let blisted = filter(range(1, bufnr('$')), 'buflisted(v:val) && v:val != btarget')
            " Listed, not target, and not displayed.
            let bhidden = filter(copy(blisted), 'bufwinnr(v:val) < 0')
            " Take the first buffer, if any (could be more intelligent).
            let bjump = (bhidden + blisted + [-1])[0]
            if bjump > 0
                execute 'buffer '.bjump
            else
                execute 'enew'
            endif
        endif
    endfor
    execute 'bdelete'.' '.btarget
    execute wcurrent.'wincmd w' 
endif]])
end)


local augroup = vim.api.nvim_create_augroup('mithic-map', {})
vim.api.nvim_create_autocmd('FileType', {
	-- TODO: use a snippet
	pattern = 'tex',
	callback = function()
		vim.keymap.set('i', '"', "''", {buffer = true})
	end,
	group = augroup,
})
