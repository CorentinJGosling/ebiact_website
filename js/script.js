const body = document.querySelector("body"),
  sidebar = body.querySelector("nav"),
  home = body.querySelector(".home"),
  hamb = body.querySelector(".menu-toggle"),
  hamb1 = body.querySelector(".one"),
  hamb2 = body.querySelector(".two"),
  hamb3 = body.querySelector(".three"),
  profile = body.querySelector(".img_profile");

var menuToggle = document.querySelector("#menu-toggle"),
  activeElements = document.querySelectorAll(".active-element");

var toggledMenu = menuToggle.addEventListener("click", function () {
  for (var activated = 0; activated < activeElements.length; activated++) {
    activeElements[activated].classList.toggle("active");
  }
});

hamb.addEventListener("click", function () {
  sidebar.classList.toggle("fullwidth");
  sidebar.classList.toggle("activate");
  profile.classList.toggle("leftMarg");
  hamb1.classList.toggle("colorMob");
  hamb3.classList.toggle("colorMob");
});

document.addEventListener("click", function (event) {
  var targetElement = event.target; // clicked element
  if (!hamb.contains(targetElement)) {
    sidebar.classList.remove("fullwidth");
    profile.classList.remove("leftMarg");
    sidebar.classList.remove("activate");
    hamb.classList.remove("active");
    hamb1.classList.remove("colorMob");
    hamb3.classList.remove("colorMob");
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
