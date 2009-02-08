
int
t (unsigned long *ret)
{
  *ret = 7856;
  return 3412;
}

void
call (unsigned long faddr)
{
  void (*func)();
  func = (void *) faddr;
  func ();
}

void
calli (unsigned long faddr, int a)
{
  void (*func)(int);
  func = (void *) faddr;
  func (a);
}

