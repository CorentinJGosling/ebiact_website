const body = document.querySelector("body"),
  sidebar = body.querySelector("nav"),
  home = body.querySelector(".home"),
  hamb = body.querySelector(".menu-toggle"),
  hamb1 = body.querySelector(".one"),
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
    hambColor.classList.remove("colorMob");
  }
});
