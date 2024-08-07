var counterElement = document.getElementById("counter");

function countUpTo(div, number) {
  var counterElement = document.querySelector(div);
  var count = 0;
  var interval = setInterval(function () {
    if (count >= number) {
      clearInterval(interval);
      counterElement.innerHTML = number;
    } else {
      count += 5;
      counterElement.innerHTML = count;
    }
  }, 40);
}

window.onload = function () {
  countUpTo(".vig1", 28);
  countUpTo(".vig2", 96);
  countUpTo(".vig3", 368);
  countUpTo(".vig4", 394);
};
ScrollReveal().reveal(".appear", { delay: 400 });
document.addEventListener("DOMContentLoaded", function () {
  fetch("img/ebiact-main.svg")
    .then((response) => response.text())
    .then((svgContent) => {
      const svgContainer = document.getElementById("svgContainer");
      svgContainer.innerHTML = svgContent;

      const svgElement = svgContainer.querySelector("svg");
      if (svgElement) {
        const figIds = [
          "fig_litterature",
          "fig_quality",
          "fig_data",
          "fig_dev",
          "fig_features",
        ];
        const textIds = [
          "text_litterature",
          "text_quality",
          "text_data",
          "text_dev",
          "text_features",
        ];
        const tooltips = {
          fig_litterature:
            "We searched the entire scientific literature to locate all the systematic reviews and meta-analyses exploring the effects of any intervention in people with ASD.",
          fig_quality:
            "Then, we appraised the quality of these scientific papers, so that we can present you the results of the current best scientific evidence.",
          fig_data:
            "We performed numerous meta-analytic calculations to estimate the effects of all interventions. We also performed a rating of the certainty of evidence, allowing you to know whether we are confident in the results of these calculations.",
          fig_dev: "All results generated are then uploaded to this platform",
          fig_features:
            "Any feedback on this platform? We need your help to tailor it to your needs!",
        };

        // Create a single tooltip element
        const tooltip = document.createElement("div");
        tooltip.className = "tooltip";
        document.getElementById("tooltipContainer").appendChild(tooltip);

        function showTooltip(event, text) {
          tooltip.innerText = text;
          tooltip.style.display = "block";
          positionTooltip(event);
        }

        function positionTooltip(event) {
          const tooltipWidth = tooltip.offsetWidth; // Get the width of the tooltip
          const tooltipHeight = tooltip.offsetHeight; // Get the height of the tooltip

          // Position the tooltip slightly to the right and below the cursor
          const offsetX = 10; // Distance from the cursor to the tooltip
          const offsetY = 10; // Distance from the cursor to the tooltip

          tooltip.style.left = `${event.pageX + offsetX}px`;
          tooltip.style.top = `${event.pageY + offsetY}px`;
        }

        function handleMouseOver(event, figId) {
          const figElement = svgElement.getElementById(figId);
          if (figElement) {
            figElement.classList.add("hovered");
            showTooltip(event, tooltips[figId] || "Tooltip text");
          }
        }

        function handleMouseOut(figId) {
          const figElement = svgElement.getElementById(figId);
          if (figElement) {
            figElement.classList.remove("hovered");
          }
          tooltip.style.display = "none";
        }

        figIds.forEach((figId) => {
          const figElement = svgElement.getElementById(figId);
          if (figElement) {
            figElement.addEventListener("mouseover", (event) =>
              handleMouseOver(event, figId)
            );
            figElement.addEventListener("mousemove", positionTooltip);
            figElement.addEventListener("mouseout", () =>
              handleMouseOut(figId)
            );
          }

          const textElement = svgElement.getElementById(
            `text_${figId.split("_")[1]}`
          );
          if (textElement) {
            textElement.addEventListener("mouseover", (event) =>
              handleMouseOver(event, figId)
            );
            textElement.addEventListener("mousemove", positionTooltip);
            textElement.addEventListener("mouseout", () =>
              handleMouseOut(figId)
            );
          }
        });
      } else {
        console.error("SVG element not found.");
      }
    })
    .catch((error) => console.error("Error loading SVG:", error));
});
