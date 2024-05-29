# Biodiversity observations: Shiny app for species mapping and timeline analysis
This Shiny application visualizes biodiversity observations from the Global Biodiversity Information Facility (GBIF). Users can explore species occurrences on an interactive map and view the timeline of observations over the years. The app focuses on observations from Poland and provides features such as species search by vernacular and scientific names, detailed observation popups, and a timeline of species sightings.

Try the live demo of the application here: [Live Demo](https://pmarcowski.shinyapps.io/global-biodiversity/).

## License
This code is licensed under the MIT license found in the LICENSE file in the root directory of this source tree.

## Features
- **Interactive map**: Visualize species observations across Poland with detailed popups including images and observation dates.
- **Species search**: Search for species by vernacular or scientific names and filter the observations on the map accordingly.
- **Observation timeline**: View a bar chart showing the frequency of species observations over time.
- **Default view**: Display all species observations for a meaningful initial view.
- **Modular design**: Built with Shiny modules for improved code maintainability and reusability.
- **Custom styling**: Enhanced UI with custom CSS for a better user experience.
- **Performance optimizations**: Efficient data handling to ensure fast initialization and responsiveness.

## Usage
The Shiny application serves as the interface for exploring biodiversity data, allowing users to:
- Enter a species name in the search box to find matching species.
- Select a species from the list to view its occurrences on the map and timeline.
- Click the "Show all species" button to reset the view and display all species observations.

## Installation
To run this application locally, follow these steps:
1. Ensure R is installed with the necessary packages.
2. Clone this repository or download the necessary files.
3. Open the *app.R* file in RStudio or another R environment.
4. Run the *app.R* script or execute the `shiny::runApp()` command in the R console.

## Data preparation
The occurrence data is filtered to include only observations from Poland, and media data (images) is joined based on observation IDs. Missing images are filled with the closest available image for the same species.

## Deployment
The app is deployed on shinyapps.io. The repository includes instructions for setting up and running the app locally.

## Feedback and questions
If you have any feedback, suggestions, or questions regarding this application, please feel free to open an issue on the GitHub repository or contact the author directly.

## Acknowledgments
This app uses data from the Global Biodiversity Information Facility (GBIF). The data is available here: [GBIF](https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165).

