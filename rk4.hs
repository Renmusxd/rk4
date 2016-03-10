import System.Environment
import Debug.Trace

-- List of [I.C.] -> t -> [Xs]
type Coord = Double
type Coords = [Coord]
type ODE = Coords->Time->Coord
type Time = Double
type ErrFun = Coords->Coords->Bool

-- Applies each function to a single argument, returns list
applyeach :: [a->b] -> a -> [b] -> [b]
applyeach (h:t) v p = applyeach t v (p++[h v])
applyeach [] v p = p

-- adds vectors and weighs second one: return v + (w*u)
mult_weight :: Double -> [Double] -> [Double] -> [Double]
mult_weight w = zipWith (\a b -> a + (w*b))

-- maps a function which takes four arguments
fourmap :: (t -> t1 -> t2 -> t3 -> a) -> [t] -> [t1] -> [t2] -> [t3] -> [a] -> [a]
fourmap f (h1:t1) (h2:t2) (h3:t3) (h4:t4) p = fourmap f t1 t2 t3 t4 (p++[f h1 h2 h3 h4])
fourmap f [] [] [] [] p = p
fourmap _ _ _ _ _ _ = error ("PM: List of different length")

-- rk4 integration single step
rk4_step :: [ODE] -> [Coord] -> Time -> Time -> [Coord]
rk4_step derivs knots t dt =
  let tbar = dt/2 in
  let d_t = map (\d c-> d c t) derivs in
  let d_tbar = map (\d c-> d c (t+tbar)) derivs in
  let d_tdt = map (\d c-> d c (t+dt)) derivs in
  let k1 = applyeach d_t knots [] in  -- For each initial condition do q1 = dq q0 t
  let k2 = applyeach d_tbar (mult_weight tbar knots k1) [] in
  let k3 = applyeach d_tbar (mult_weight tbar knots k2) [] in
  let k4 = applyeach d_tdt (mult_weight dt knots k3) [] in
  let toadd = fourmap (\a b c d -> (1/6)*dt*(a + (2*b) + (2*c) + d)) k1 k2 k3 k4 [] in
  zipWith (+) knots toadd

-- rk4 integration
rk4 :: [ODE] -> Coords -> Time -> Time -> Time -> Coords
rk4 derivs ics start_t end_t step_size =
  let time_table = [start_t,step_size .. end_t] in
  foldl rk4_step_fun ics time_table
  where rk4_step_fun k0s t = rk4_step derivs k0s t step_size

-- rk4 variable
-- ODES -> I.C. -> Err -> Start_T -> End_T -> I.dt -> Out
rk4v :: [ODE] -> Coords -> ErrFun -> Time -> Time -> Time -> Coords
rk4v derivs ics errf start_t end_t step_size =
  let c1 = rk4 derivs ics start_t end_t step_size in
  let c2 = rk4 derivs ics start_t end_t (step_size/2.0) in
  if errf c1 c2 then rk4v derivs ics errf start_t end_t (step_size/2.0)
  else c2

-- rk4 step variable
rk4sv :: [ODE] -> Coords -> ErrFun -> Time -> Time -> Time -> Coords
rk4sv derivs ics errf start_t end_t step_size =
  let use_dt = min (end_t - start_t) step_size in
  let basec = rk4_step derivs ics start_t use_dt in
  let (newc, newdt) = rk4sv_helper ics basec start_t use_dt in
  if (start_t+newdt >= end_t) then
    newc
  else
    rk4sv derivs newc errf (start_t+newdt) end_t newdt  
  where
    -- rk4sv_helper :: Coords -> Coords -> Time -> Time -> (Coords, Time)
    rk4sv_helper basec intc t dt =
      let halfc = rk4_step derivs basec t (dt/2.0) in
      let c2 = rk4_step derivs halfc (t+(dt/2.0)) (dt/2.0) in
      if errf intc c2 then
        rk4sv_helper basec c2 t (dt/2.0)
      else
        (c2, dt)    

            

main :: IO()
main = print (show (rk4 [\xs t -> head xs] [1] 0 1 0.001))


