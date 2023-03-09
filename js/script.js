const body = document.querySelector("body"),
  sidebar = body.querySelector("nav"),
  home = body.querySelector(".home");
// hamb = body.querySelector(".hamburger");
const svgElement = document.getElementById("hamb");
const svgElementT = document.getElementById("top");
const svgElementB = document.getElementById("bottom");
const svgElementM = document.getElementById("middle");
const profile = body.querySelector(".img_profile");
svgElement.addEventListener("click", function () {
  svgElement.classList.toggle("open");
  svgElement.classList.toggle("inc_width");
  sidebar.classList.toggle("activate");
  profile.classList.toggle("leftMarg");
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
  }, 50);
}

window.onload = function () {
  countUpTo(".vig1", 1);
  countUpTo(".vig2", 44);
  countUpTo(".vig3", 128);
  countUpTo(".vig4", 192);
};
ScrollReveal().reveal(".appear", { delay: 400 });
