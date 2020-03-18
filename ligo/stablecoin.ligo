type parameter is nat

type storage is nat

type return is list (operation) * storage

function main (const action : parameter; const store : storage) : return is
  ((nil : list(operation)), action)
