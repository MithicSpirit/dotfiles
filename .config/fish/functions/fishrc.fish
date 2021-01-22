# Defined via `source`
function fishrc --wraps='nvim ~/.config/fish/config.fish' --description 'alias fishrc nvim ~/.config/fish/config.fish'
  nvim ~/.config/fish/config.fish $argv; 
end
