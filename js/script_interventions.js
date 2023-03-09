const body = document.querySelector("body"),
  sidebar = body.querySelector("nav"),
  home = body.querySelector(".home");

const svgElement = document.getElementById("hamb");
const svgElementT = document.getElementById("top");
const svgElementB = document.getElementById("bottom");
const svgElementM = document.getElementById("middle");
const profile = body.querySelector(".img_profile");
svgElement.addEventListener("click", function () {
  svgElement.classList.toggle("open");
  svgElement.classList.toggle("inc_width");
  profile.classList.toggle("leftMarg");
  sidebar.classList.toggle("activate");
  svgElementT.classList.toggle("colorMob");
  svgElementB.classList.toggle("colorMob");
  svgElementM.classList.toggle("colorMob");
  if (sidebar.classList.contains("activate")) {
    sidebar.classList.remove("close");
    sidebar.classList.add("fullwidth");
  } else {
    sidebar.classList.remove("fullwidth");
    sidebar.classList.add("close");
  }
});
