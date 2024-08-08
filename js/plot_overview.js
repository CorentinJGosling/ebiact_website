function createScatterPlot(dataFileName) {
  fetch(dataFileName)
    .then((response) => {
      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }
      return response.json();
    })
    .then((data) => {
      // Find the maximum radius in the main dataset
      const maxRadius = Math.max(...data.main.map((item) => item.r));

      // Determine the spacing needed between each tick
      const stepSize = maxRadius * 2;

      // Create the scatter plot
      var ctx = document.getElementById("myScatterPlot").getContext("2d");
      var scatterPlot = new Chart(ctx, {
        type: "scatter",
        data: {
          datasets: [
            // Background dataset to create space between dots and borders
            {
              label: "Background Dots",
              data: data.background.map((item) => ({
                x: item.x,
                y: item.y,
              })),
              backgroundColor: data.background.map(
                (item) => item.backgroundColor
              ),
              borderColor: data.background.map((item) => item.borderColor),
              borderWidth: data.background.map((item) => item.borderWidth),
              pointRadius: data.background.map((item) => item.pointRadius),
              hoverRadius: data.background.map((item) => item.pointRadius),
              hoverBackgroundColor: data.background.map(
                (item) => item.backgroundColor
              ),
              hoverBorderColor: data.background.map((item) => item.borderColor),
              hoverBorderWidth: data.background.map((item) => item.borderWidth),
            },
            // Main dataset with different colors, sizes, and borders

            // Star dataset, rendered last to be on top
            {
              label: "Star Dataset",
              data: data.star.map((item) => ({
                x: item.x,
                y: item.y,
              })),
              pointRadius: data.star.map((item) => item.pointRadius),
              pointStyle: "star", // Star-shaped points
              backgroundColor: "rgba(255, 99, 132, 1)", // Fixed color for star points
              borderColor: "rgba(255, 99, 132, 1)", // Fixed border color for star points
              hoverRadius: 10, // Maintain radius on hover
              hoverBackgroundColor: "rgba(255, 99, 132, 1)", // Maintain background color on hover
              hoverBorderColor: "rgba(255, 99, 132, 1)", // Maintain border color on hover
              hoverBorderWidth: 3,
            },
            {
              label: "Main Dataset",
              data: data.main.map((item) => ({
                x: item.x,
                y: item.y,
                r: item.r,
                backgroundColor: item.backgroundColor,
                borderColor: item.borderColor,
                borderWidth: item.borderWidth,
              })),
              pointRadius: function (context) {
                return context.raw.r; // Use the radius from data
              },
              backgroundColor: function (context) {
                return context.raw.backgroundColor; // Use the background color from data
              },
              borderColor: function (context) {
                return context.raw.borderColor; // Use the border color from data
              },
              borderWidth: function (context) {
                return context.raw.borderWidth; // Use the border width from data
              },
              hoverRadius: function (context) {
                return context.raw.r; // Maintain radius on hover
              },
              hoverBackgroundColor: function (context) {
                return context.raw.backgroundColor; // Maintain background color on hover
              },
              hoverBorderColor: function (context) {
                return context.raw.borderColor; // Maintain border color on hover
              },
              hoverBorderWidth: function (context) {
                return context.raw.borderWidth; // Maintain border width on hover
              },
            },
          ],
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,

          scales: {
            x: {
              offset: true,
              type: "category", // Use category type for character-based axis
              labels: data.xLabels, // Custom labels for x-axis from JSON
              position: "top",
              ticks: {
                padding: 20, // Add padding to create space
                font: {
                  weight: "bold", // Make x-axis ticks bold
                  size: 18,
                },
              },
            },
            y: {
              type: "category", // Use category type for character-based axis

              labels: data.yLabels, // Custom labels for y-axis from JSON
              position: "left",
              ticks: {
                padding: 10, // Increase padding between ticks based on maxRadius
                stepSize: 100, // Set the step size based on the largest dot radius
              },
            },
          },
          plugins: {
            legend: {
              display: false, // Disable the legend
            },
            tooltip: {
              enabled: true,
              displayColors: false, // Disable color box in tooltip
              callbacks: {
                label: function (tooltipItem) {
                  // Check if the dataset is the main dataset (index 1) or star dataset (index 2)
                  if (
                    tooltipItem.datasetIndex === 1 ||
                    tooltipItem.datasetIndex === 2
                  ) {
                    return ""; // Return empty string to hide tooltip for main and star datasets
                  } else {
                    return `x: ${tooltipItem.raw.x}<br>y: ${tooltipItem.raw.y}`;
                  }
                },
              },
            },
          },
        },
      });
    })
    .catch((error) => console.error("Error fetching the dataset:", error));
}
