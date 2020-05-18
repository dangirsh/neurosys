HOST=$1
HOST_HOME=$2

rsync -Pav --rsync-path="sudo rsync" nixos/ $HOST:/etc/nixos/
rsync -Pav home/ $HOST:$HOST_HOME
