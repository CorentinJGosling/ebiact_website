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

      // Create the scatter plot
      var ctx = document.getElementById("myScatterPlot").getContext("2d");
      var scatterPlot = new Chart(ctx, {
        type: "scatter",
        data: {
          datasets: [
            {
              label: "Background dashed Dots",
              data: data.background_dashed.map((item) => ({
                x: item.x,
                y: item.y,
                customLabel: item.customLabel,
                n_studies: item.n_studies,
              })),
              backgroundColor: data.background_dashed.map(
                (item) => item.backgroundColor
              ),
              borderColor: data.background_dashed.map(
                (item) => item.borderColor
              ),
              borderWidth: data.background_dashed.map(
                (item) => item.borderWidth
              ),
              pointRadius: data.background_dashed.map(
                (item) => item.pointRadius
              ),
              hoverRadius: data.background_dashed.map(
                (item) => item.pointRadius + 5
              ), // Adjust hover radius for smooth effect
              hoverBackgroundColor: data.background_dashed.map(
                (item) => item.hoverBackgroundColor
              ),
              hoverBorderColor: data.background_dashed.map(
                (item) => item.borderColor
              ),
              hoverBorderWidth: data.background_dashed.map(
                (item) => item.borderWidth
              ),
              pointStyle: function (ctx) {
                var radius = ctx.dataset.pointRadius[ctx.dataIndex];
                var dashLength = 5;
                var dashGap = 6;

                var canvas = document.createElement("canvas");
                canvas.width = radius * 2;
                canvas.height = radius * 2;
                var customCtx = canvas.getContext("2d");

                customCtx.beginPath();
                customCtx.arc(radius, radius, radius - 1, 0, Math.PI * 2);
                customCtx.setLineDash([dashLength, dashGap]);
                customCtx.lineWidth = 2;
                customCtx.strokeStyle = ctx.dataset.borderColor[ctx.dataIndex];
                customCtx.stroke();

                return canvas;
              },
            },
            {
              label: "Background Dots",
              data: data.background.map((item) => ({
                x: item.x,
                y: item.y,
                customLabel: item.customLabel,
                n_studies: item.n_studies,
              })),
              backgroundColor: data.background.map(
                (item) => item.backgroundColor
              ),
              borderColor: data.background.map((item) => item.borderColor),
              borderWidth: data.background.map((item) => item.borderWidth),
              pointRadius: data.background.map((item) => item.pointRadius),
              hoverRadius: data.background.map((item) => item.pointRadius + 2),
              hoverBackgroundColor: data.background.map(
                (item) => item.backgroundColor
              ),
              hoverBorderColor: data.background.map((item) => item.borderColor),
              hoverBorderWidth: data.background.map((item) => item.borderWidth),
            },
            {
              label: "Star Dataset",
              data: data.star.map((item) => ({
                x: item.x,
                y: item.y,
              })),
              pointRadius: data.star.map((item) => item.pointRadius),
              pointStyle: "star",
              backgroundColor: "rgba(255, 99, 132, 1)",
              borderColor: "rgba(255, 99, 132, 1)",
              hoverRadius: 10,
              hoverBackgroundColor: "rgba(255, 99, 132, 1)",
              hoverBorderColor: "rgba(255, 99, 132, 1)",
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
                customLabel: item.customLabel,
                borderWidth: item.borderWidth,
              })),
              pointRadius: function (context) {
                return context.raw.r;
              },
              backgroundColor: function (context) {
                return context.raw.backgroundColor;
              },
              borderColor: function (context) {
                return context.raw.borderColor;
              },
              borderWidth: function (context) {
                return context.raw.borderWidth;
              },
              hoverRadius: function (context) {
                return context.raw.r;
              },
              hoverBorderColor: function (context) {
                return context.raw.borderColor;
              },
              hoverBorderWidth: function (context) {
                return context.raw.borderWidth;
              },
            },
          ],
        },
        options: {
          maintainAspectRatio: false,
          responsive: true,
          animation: {
            duration: 1000, // Duration in milliseconds
            easing: "easeInOutCubic", // Easing function for smooth transitions
          },
          onClick: function (event, chartElement) {
            if (chartElement.length) {
              const datasetIndex = chartElement[0].datasetIndex;
              const dataIndex = chartElement[0].index;

              const customLabel =
                this.data.datasets[datasetIndex].data[dataIndex].customLabel;

              // Set the content of the modal
              const modalBody = document.getElementById("modalBody");
              modalBody.innerHTML = customLabel;

              // Get the modal and show it
              const modal = document.getElementById("customLabelModal");
              modal.style.display = "block";

              // Close the modal when the user clicks on <span> (x)
              const closeButton = document.querySelector(".close-button");
              closeButton.onclick = function () {
                modal.style.display = "none";
              };

              // Close the modal when the user clicks anywhere outside of the modal
              window.onclick = function (event) {
                if (event.target === modal) {
                  modal.style.display = "none";
                }
              };
            }
          },

          // onClick: function (event, chartElement) {
          //   if (chartElement.length) {
          //     const datasetIndex = chartElement[0].datasetIndex;
          //     const dataIndex = chartElement[0].index;

          //     const customLabel =
          //       this.data.datasets[datasetIndex].data[dataIndex].customLabel;

          //     const labelCard = document.getElementById("labelCard");
          //     labelCard.innerHTML = customLabel;
          //     labelCard.style.display = "block";

          //     const labelDefault = document.getElementById("labelDefault");
          //     labelDefault.style.display = "none";
          //   }
          // },
          onHover: function (event, chartElement) {
            if (chartElement.length) {
              event.native.target.style.cursor = "pointer";
            } else {
              event.native.target.style.cursor = "default";
            }
          },
          scales: {
            x: {
              offset: true,
              type: "category",
              labels: data.xLabels,
              position: "top",
              ticks: {
                padding: 20,
                font: {
                  weight: "bold",
                  size: 18,
                },
              },
            },
            y: {
              type: "category",
              labels: data.yLabels,
              position: "left",
              ticks: {
                padding: 10,
              },
            },
          },
          plugins: {
            legend: {
              display: false,
            },
            tooltip: {
              enabled: false,
              label: "click me",

              external: function (context) {
                var tooltipModel = context.tooltip;
                var tooltipEl = document.getElementById("chartjs-tooltip");

                if (!tooltipEl) {
                  tooltipEl = document.createElement("div");
                  tooltipEl.id = "chartjs-tooltip";
                  tooltipEl.innerHTML = "<table></table>";
                  document.body.appendChild(tooltipEl);
                }

                if (
                  tooltipModel.opacity === 0 ||
                  !tooltipModel.dataPoints ||
                  (tooltipModel.dataPoints[0].datasetIndex !== 1 &&
                    tooltipModel.dataPoints[0].datasetIndex !== 0)
                ) {
                  tooltipEl.style.opacity = 0;
                  return;
                }

                tooltipEl.classList.remove("above", "below", "no-transform");
                if (tooltipModel.yAlign) {
                  tooltipEl.classList.add(tooltipModel.yAlign);
                } else {
                  tooltipEl.classList.add("no-transform");
                }

                if (tooltipModel.body) {
                  var n_studies =
                    tooltipModel.dataPoints[0].raw.n_studies ||
                    "No label provided";

                  var innerHtml =
                    "<thead><tr><th>Number of studies: <b>" +
                    n_studies +
                    "</b></th></tr></thead>";

                  var tableRoot = tooltipEl.querySelector("table");
                  tableRoot.innerHTML = innerHtml;
                }

                var position = context.chart.canvas.getBoundingClientRect();
                tooltipEl.style.opacity = 1;
                tooltipEl.style.position = "absolute";
                tooltipEl.style.left =
                  position.left +
                  window.pageXOffset +
                  tooltipModel.caretX +
                  "px";
                tooltipEl.style.top =
                  position.top +
                  window.pageYOffset +
                  tooltipModel.caretY +
                  "px";
                tooltipEl.style.font = tooltipModel.options.bodyFont.string;
                tooltipEl.style.padding =
                  tooltipModel.options.padding +
                  "px " +
                  tooltipModel.options.padding +
                  "px";
                tooltipEl.style.pointerEvents = "none";
              },
            },
          },
        },
      });
      function updateChartOptions() {
        if (window.innerWidth < 768) {
          scatterPlot.options.scales.x.offset = false; // Disable offset for small screens
          scatterPlot.options.scales.x.ticks.maxRotation = 60; // Rotate labels by 90 degrees
          scatterPlot.options.scales.x.ticks.minRotation = 60; // Ensure consistent rotation
          scatterPlot.options.scales.x.ticks.font.size = 14; // Ensure consistent rotation

          scatterPlot.data.datasets.forEach((dataset) => {
            dataset.pointRadius = dataset.pointRadius.map(
              (radius) => radius * 0.5 // Scale down by 50%
            );
          });
        } else {
          scatterPlot.options.scales.x.offset = true; // Enable offset for larger screens
          scatterPlot.options.scales.x.ticks.maxRotation = 0; // No rotation
          scatterPlot.options.scales.x.ticks.minRotation = 0; // No rotation
          scatterPlot.options.scales.x.ticks.font.size = 20; // Ensure consistent rotation
        }
        scatterPlot.update(); // Apply the changes
      }

      // Initial call to update options based on the current window size
      updateChartOptions();

      // Add an event listener to update the chart options on window resize
      window.addEventListener("resize", updateChartOptions);
    })
    .catch((error) => console.error("Error fetching the dataset:", error));
}
