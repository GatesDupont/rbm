# eBird Rarity Viewer

## About the App

eBird Rarity Viewer is a Shiny web application designed for birders. It leverages eBird data to provide an interactive and intuitive visualization of recent rare bird sightings anywhere in the world. Users can explore sightings on a map, filter by region, date, or species, and dive deep into briding data analytics!


## Getting Started

### Prerequisites

- R (version 4.2.1 or higher)
- RStudio


### Installation

1. Clone the repository: 
```bash
git clone https://github.com/GatesDupont/rbm.git
```

2. Open the project in RStudio.
3. Install the required R packages:

```R
install.packages(c("shiny", "tidyverse", "leaflet", ...))
```


### Setting Up API Key

1. Obtain an API key from [eBird](https://documenter.getpostman.com/view/664302/S1ENwy59).
2. Create a file named `config.yml` in the root directory of the project.
3. Add your API key to `config.yml`:

```YML
default:
  ebird_api_key: "your_api_key_here"
```

## Usage

- Open the app in RStudio and run it.
- The interface is intuitive: select your filters and observe the map and dropdown menu updating in real time.

## License

This project is licensed under the MIT License.

## Acknowledgments

- Thanks to the eBird team for providing the API.



