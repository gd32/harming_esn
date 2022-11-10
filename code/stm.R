# Making a state transition diagram

library('heemod')
library('diagram')
mat_dim <- define_transition(
  state_names = c
  ('Peace', 'Not Peace'),
  .95, .05,
  .05, .95)
plot(mat_dim,
     box.size = 0.125,
     self.shiftx = c(-0.1, 0.1),
     self.shifty = c(-0.15, 0.15))
