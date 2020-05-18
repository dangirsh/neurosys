HOST=$1

rsync -Pav --rsync-path="sudo rsync" nixos/ $HOST:/etc/nixos/
rsync -Pav home/ $HOST:/home/dan/
