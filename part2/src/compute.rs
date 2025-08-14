pub type Pair = [[f64; 2]; 2];
pub type PairIter = Box<dyn Iterator<Item = Pair>>;

pub const EARTH_RADIUS: f64 = 6372.8;

pub fn reference_haversine([[x0, y0], [x1, y1]]: [[f64; 2]; 2], earth_radius: f64) -> f64 {
  let lat1 = y0;
  let lat2 = y1;
  let lon1 = x0;
  let lon2 = x1;

  let dlat = (lat2 - lat1).to_radians();
  let dlon = (lon2 - lon1).to_radians();
  let lat1 = (lat1).to_radians();
  let lat2 = (lat2).to_radians();

  let a = (dlat / 2.0).sin().powi(2) + lat1.cos() * lat2.cos() * (dlon / 2.0).sin().powi(2);
  let c = 2.0 * a.sqrt().asin();

  earth_radius * c
}
