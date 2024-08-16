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
  }, 25);
}

window.onload = function () {
  countUpTo(".vig1", 28);
  countUpTo(".vig2", 96);
  countUpTo(".vig3", 368);
  countUpTo(".vig4", 394);
};
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
const svgPath2 = "../img/Frame1.svg"; // Path to your SVG file
const svgContainer2 = document.getElementById("svgContainer2"); // Container to load SVG

// Fetch the SVG file
fetch(svgPath2)
  .then((response) => response.text())
  .then((svgText) => {
    // Insert the SVG into the container
    svgContainer2.innerHTML = svgText;

    // Find the element with the ID 'Frame1'
    const frameElement = svgContainer2.querySelector("#Frame1");

    if (frameElement) {
      console.log("Frame 1 element found:", frameElement);

      // Find the 'screen' element within 'Frame1'
      const screenElement = frameElement.querySelector("#screen");

      if (screenElement) {
        console.log("Screen element found:", screenElement);

        // Get the dimensions and position of the 'screen' element
        const { x, y, width, height } = screenElement.getBBox();
        console.log(
          `Screen element dimensions: x=${x}, y=${y}, width=${width}, height=${height}`
        );

        // Create a foreignObject element
        const foreignObject = document.createElementNS(
          "http://www.w3.org/2000/svg",
          "foreignObject"
        );
        foreignObject.setAttribute("x", x);
        foreignObject.setAttribute("y", y);
        foreignObject.setAttribute("width", width);
        foreignObject.setAttribute("height", height);

        // Create a div inside foreignObject to hold the video and play button
        const contentDiv = document.createElement("div");
        contentDiv.style.position = "relative";
        contentDiv.style.width = "100%";
        contentDiv.style.height = "100%";
        contentDiv.style.display = "flex";
        contentDiv.style.alignItems = "center";
        contentDiv.style.justifyContent = "center";
        contentDiv.style.overflow = "hidden";

        // Create a video element
        const video = document.createElement("video");
        video.src = "../img/W4SQUS692FCWXGBX.mp4"; // Replace with your video URL
        video.controls = true; // Show default video controls
        video.style.width = "100%";
        video.style.height = "100%";
        video.style.objectFit = "contain"; // Ensure the video scales within the div

        // Create a play button element
        const playButton = document.createElement("div");
        playButton.className = "play-button"; // Class for styling
        playButton.style.position = "absolute";
        playButton.style.width = "60px"; // Size of the play button
        playButton.style.height = "60px";
        playButton.style.background = "rgba(0, 0, 0, 0.7)"; // Semi-transparent background
        playButton.style.borderRadius = "50%";
        playButton.style.display = "flex";
        playButton.style.alignItems = "center";
        playButton.style.justifyContent = "center";
        playButton.style.cursor = "pointer";
        playButton.style.color = "#fff";
        playButton.style.fontSize = "24px";
        playButton.style.fontFamily = "Arial, sans-serif";
        playButton.innerHTML = "▶"; // Play icon (could be replaced with an SVG or image)

        // Add event listener to the play button
        playButton.addEventListener("click", () => {
          video.play();
          playButton.style.display = "none"; // Hide play button after play
        });

        // Append the video and play button to the div
        contentDiv.appendChild(video);
        contentDiv.appendChild(playButton);

        // Append the div to the foreignObject
        foreignObject.appendChild(contentDiv);

        // Clear the 'screen' element and replace it with foreignObject
        screenElement.innerHTML = "";
        screenElement.parentNode.replaceChild(foreignObject, screenElement);

        console.log(
          "Video and custom play button embedded into the screen element."
        );
      } else {
        console.error(
          'No element with the ID "screen" found within "Frame 1".'
        );
      }
    } else {
      console.error('No element with the ID "Frame 1" found in the SVG.');
    }
  })
  .catch((error) => {
    console.error("Error loading the SVG file:", error);
  });
