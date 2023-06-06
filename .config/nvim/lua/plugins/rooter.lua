vim.g.rooter_patterns = {
	'.git',
	'_darcs',
	'.hg',
	'.bzr',
	'.svn',
	'Makefile',
	'package.json',
	'Cargo.toml',
	'.projectile',
	'>.config',
	'>.local/share',
	'>coding',
}
vim.g.rooter_cd_cmd = 'tcd'
vim.g.rooter_change_directory_for_non_project_files = 'current'
