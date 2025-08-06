import requests
import json
import re
# Generates Tags by iterating through the umm_json file and searching for key words
url = "https://cmr.earthdata.nasa.gov/search/concepts/C2105092163-LAADS.umm_json" \
""
array = ["1940 census", "1950 census", "2020 census", "aerial imagery", "agriculture", "air quality", "air temperature", "analysis ready data", "atmosphere", "bathymetry", "biodiversity", "carbon", "census", "climate", "climate model","climate projections","climate risk","CMIP5","CMIP6","coastal","cog","contamination",
            "coronavirus",
            "COVID-19",
            "cropland partitioning",
            "cyclone",
            "typhoon",
            "hurricane",
            "cyclone typhoon hurricane",
            "cyclone",
            "typhoon",
            "hurricane",
            "datacenter",
            "data center",
            "demographics",
            "disaster response",
            "earth observation",
            "earthquakes",
            "economics",
            "ecosystems",
            "energy",
            "energy modeling",
            "environmental",
            "ethnicity",
            "evapotranspiration",
            "extreme weather",
            "floods",
            "food security",
            "forecast",
            "geology",
            "geophysics",
            "geoscience",
            "geospatial",
            "geothermal",
            "global",
            "ground water",
            "hydrography",
            "hydrologic model",
            "hydrology",
            "ice",
            "irrigated cropland",
            "land",
            "land cover",
            "land use",
            "lidar",
            "marine",
            "metadata",
            "natural resource",
            "near-surface air temperature",
            "near-surface relative humidity",
            "near-surface specific humidity",
            "ocean circulation",
            "ocean currents",
            "ocean velocity",
            "ocean sea surface height",
            "ocean simulation",
            "oceans",
            "opendap",
            "orbit",
            "parquet",
            "politics",
            "population",
            "radar",
            "rainfed cropland",
            "SARS",
            "SARS-CoV-2",
            "satellite imagery",
            "seafloor",
            "seismology",
            "sentinel-1",
            "socioeconomic",
            "soil moisture",
            "stac ALL",
            "surface water",
            "synthetic aperture radar",
            "tiles",
            "time series forecasting",
            "urban",
            " us ",
            "water",
            "xml",
            "zarr",
            "wind speeds",
            "cloud amount"]

resp = requests.get(url)
umm = resp.json()

def flatten(obj):
    if isinstance(obj, dict):
        return " ".join(flatten(v) for v in obj.values())
    elif isinstance(obj, list):
        return " ".join(flatten(v) for v in obj)
    elif isinstance(obj, str):
        return obj
    else:
        return ""
    
umm_text = flatten(umm).lower()
raw_matches = [kw for kw in array if re.search(rf'\b{re.escape(kw.lower())}\b', umm_text)]
matches = []
matches.append("aws-pds")
for kw in raw_matches:
    if kw == "cyclone":
        matches.append("cyclone typhoon hurricane")
    elif kw == "typhoon":
        matches.append("cyclone typhoon hurricane")
    elif kw == "hurricane":
        matches.append("cyclone typhoon hurricane")
    elif kw == "data center":
        matches.append("datacenter")
    elif kw == " us ":
        matches.append("us")
    else:
        matches.append(kw)
print("Matches:", matches)
print("\nFormatted list:")
for item in matches:
    print(f"  - {item}")