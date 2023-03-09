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
  svgElementT.classList.toggle("colorMob");
  svgElementB.classList.toggle("colorMob");
  svgElementM.classList.toggle("colorMob");
  svgElement.classList.toggle("open");
  sidebar.classList.toggle("fullwidth");
  svgElement.classList.toggle("inc_width");
  sidebar.classList.toggle("activate");
  profile.classList.toggle("leftMarg");
});
// if (sidebar.classList.contains("activate")) {
//   sidebar.classList.remove("close");
//   sidebar.classList.add("fullwidth");
// } else {
//   sidebar.classList.remove("fullwidth");
//   sidebar.classList.add("close");
// }

window.onclick = function (e) {
  if (e.target == sidebar || e.target == svgElement) {
    return;
  } else {
    sidebar.classList.remove("fullwidth");
    sidebar.classList.add("close");
    svgElement.classList.remove("inc_width");
    profile.classList.remove("leftMarg");
    svgElement.classList.remove("open");
    sidebar.classList.remove("activate");
    svgElementT.classList.remove("colorMob");
    svgElementB.classList.remove("colorMob");
    svgElementM.classList.remove("colorMob");
  }
};

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
