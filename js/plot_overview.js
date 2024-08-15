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
                (item) => item.pointRadius
              ),
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
                customCtx.strokeStyle = "rgba(255, 99, 132, 1)";
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
                customLabel: item.customLabel, // Ensure this is included
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
          aspectRatio: 1.4,
          onClick: function (event, chartElement) {
            if (chartElement.length) {
              const datasetIndex = chartElement[0].datasetIndex;
              const dataIndex = chartElement[0].index;

              const customLabel =
                this.data.datasets[datasetIndex].data[dataIndex].customLabel;

              // Display the custom label in the card
              const contain_helper = document.getElementById("contain_helper");
              contain_helper.style.display = "flex";

              // Display the custom label in the card
              const labelCard = document.getElementById("labelCard");
              labelCard.innerHTML = customLabel;
              labelCard.style.display = "block";
            }
          },
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
                  var customLabel =
                    tooltipModel.dataPoints[0].raw.customLabel ||
                    "No label provided";

                  var innerHtml =
                    "<thead><tr><th>" + customLabel + "</th></tr></thead>";

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
    })
    .catch((error) => console.error("Error fetching the dataset:", error));
}
