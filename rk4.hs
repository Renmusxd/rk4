import System.Environment
import Debug.Trace

-- List of [I.C.] -> t -> [Xs]
type Coord = (Double,Double,Double)
type ODE = Coord->Double->Double
type SysODEs = (ODE,ODE,ODE)
type Time = Double
-- Takes a list of functions x', y', z', ...
-- and a list of initial conditions x0, y0, z0, ...
-- plus a time step dt
-- Returns a list of new values
rk4_poc :: SysODEs -> Coord -> Time -> Time -> Coord
rk4_poc derivs knots t dt =
  let (dx,dy,dz) = derivs in
  let (x0,y0,z0) = knots in
  let x1 = dx knots t in
  let y1 = dy knots t in
  let z1 = dz knots t in
  let tbar = dt/2 in
  let inc_coord1 = (x0+tbar*x1,y0+tbar*y1,z0+tbar*z1) in
  let x2 = dx inc_coord1 (t+tbar) in
  let y2 = dy inc_coord1 (t+tbar) in
  let z2 = dz inc_coord1 (t+tbar) in
  let inc_coord2 = (x0+tbar*x2,y0+tbar*y2,z0+tbar*z2) in
  let x3 = dx inc_coord2 (t+tbar) in
  let y3 = dy inc_coord2 (t+tbar) in
  let z3 = dz inc_coord2 (t+tbar) in
  let inc_coord3 = (x0+dt*x3,y0+dt*y3,z0+dt*z3) in
  let x4 = dx inc_coord3 (t+dt) in
  let y4 = dy inc_coord3 (t+dt) in
  let z4 = dz inc_coord3 (t+dt) in
  -- Finally put it all together
  let xn = (1/6)*(x1 + 2*x2 + x3 + x4) in
  let yn = (1/6)*(y1 + 2*y2 + y3 + y4) in
  let zn = (1/6)*(z1 + 2*z2 + z3 + z4) in
  (xn,yn,zn)

applyeach :: [a->b] -> a -> [b] -> [b]
applyeach (h:t) v p = applyeach t v (p++[h v])
applyeach [] v p = p

mult_weight w (h1:t1) (h2:t2) p = mult_weight w t1 t2 (p++[h1 + w*h2]);
mult_weight w [] [] p = p;
mult_weight w l1 l2 p = error ("MW: Different length "++(show l1)++" "++(show l2))

fourmap f (h1:t1) (h2:t2) (h3:t3) (h4:t4) p = fourmap f t1 t2 t3 t4 (p++[f h1 h2 h3 h4])
fourmap f [] [] [] [] p = p
fourmap _ _ _ _ _ _ = error ("PM: List of different length")


rk4_step :: [[Double]->Double->Double] -> [Double] -> Time -> Time -> [Double]
rk4_step derivs knots t dt =
  let tbar = dt/2 in
  let tn = t + dt in
  let d_t = map (\d c-> d c t) derivs in
  let d_tbar = map (\d c-> d c (t+tbar)) derivs in
  let d_tdt = map (\d c-> d c (t+dt)) derivs in
  let k1 = applyeach d_t knots [] in  -- For each initial condition do q1 = dq q0 t
  let k2 = applyeach d_tbar (mult_weight tbar knots k1 []) [] in
  let k3 = applyeach d_tbar (mult_weight tbar knots k2 []) [] in
  let k4 = applyeach d_tdt (mult_weight tn knots k3 []) [] in
  fourmap (\a b c d -> (1/6)*(a + (2*b) + c + d)) k1 k2 k3 k4 []

rk4 :: [[Double]->Double->Double] -> [Double] -> Time -> Time -> Time -> [Double]
rk4 derivs ics start_t end_t step_size =
  let time_table = [start_t,step_size .. end_t] in
  foldr rk4_step_fun ics time_table
  where rk4_step_fun t k0s =
          trace (show k0s) (rk4_step derivs k0s t step_size)
  
-- x'' + ax' + bsin(x) = F(t)


main :: IO()
main = print "rk4"


