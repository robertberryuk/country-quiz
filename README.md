# Where on Earth? — minimalist country quiz

A lightweight, self-contained **R Shiny** app built for the **#30DayMapChallenge** (Day 5: Minimal map).  
It drops a red pin somewhere on Earth and asks you to guess the country.

---

## what this is

- **Lightweight:** one `app.R`, no server-side database, no user data stored.
- **Self-contained:** first run downloads small Natural Earth layers and **caches** the countries table to `data/ne_admin0_small.rds` (auto-created, and `.gitignore`d).
- **Minimalist styling:** sea light blue, land white, dark coastline, no basemap tiles.
- **Robust loaders:** all data downloads wrapped in `try(..., silent = TRUE)`; graceful fallbacks on failure.
- **Quiet console:** avoids noisy `st_union` and bbox warnings by using the NE *coastline* layer and a clamped sea rectangle.
- **Clean reactivity:** helper functions use `isolate()` to avoid reactive context surprises.

---

## how it works

1. On first run, the app:
   - Downloads **Natural Earth**: `land`, `coastline`, and **admin-0 countries** via `rnaturalearth`.
   - Validates and reprojects to EPSG:4326.
   - Writes a trimmed countries object to **RDS** at `data/ne_admin0_small.rds` (columns: `iso_a3`, `name`, `continent`, `geometry`).
2. Each round:
   - Randomly selects a country polygon and samples a point on or within it.
   - Places a red marker; you pick a country from a searchable dropdown.
   - Score, streak, and rounds update; **Next** resets the map to full extent.

**No datasets are stored in the repository or a database.** The RDS cache is created locally on the server and is safe to delete; it will be rebuilt on demand.

---

## run locally

```r
# install once
install.packages(c("shiny","leaflet","sf","rnaturalearth","rnaturalearthdata","bslib"))

# run
shiny::runApp("country-quiz")   # or open app.R and click 'Run App'
