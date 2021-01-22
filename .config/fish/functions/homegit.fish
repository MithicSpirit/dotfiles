# Defined via `source`
function homegit --wraps='git --git-dir=$HOME/.homegit --work-tree=$HOME' --description 'alias homegit git --git-dir=$HOME/.homegit --work-tree=$HOME'
  git --git-dir=$HOME/.homegit --work-tree=$HOME $argv; 
end
