
type power_flag = Until | WeakUntil | Release | StrongRelease

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t
}
