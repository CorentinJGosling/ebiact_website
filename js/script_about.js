// Wrap every letter in a span
var textWrapper = document.querySelector(".ml6 .letters");
textWrapper.innerHTML = textWrapper.textContent.replace(
  /\S/g,
  "<span class='letter'>$&</span>"
);
setTimeout(function () {
  anime.timeline({ loop: false }).add({
    targets: ".ml6 .letter",
    translateY: ["4rem", 0],
    translateZ: 0,
    duration: 750,
    delay: (el, i) => 20 * i,
  });
}, 2500);
// .add({
//   targets: ".ml6",
//   opacity: 0,
//   duration: 1000,
//   easing: "easeOutExpo",
//   delay: 1000,
// });
var toggleButtonCG = document.getElementById("readmore_cg");
var textCG = document.getElementById("read_more_text_cg");
var closeiconCG = document.getElementById("close_cg");
var openiconCG = document.getElementById("open_cg");
var toggleButtonAC = document.getElementById("readmore_ac");
var textAC = document.getElementById("read_more_text_ac");
var closeiconAC = document.getElementById("close_ac");
var openiconAC = document.getElementById("open_ac");
var toggleButtonRD = document.getElementById("readmore_rd");
var textRD = document.getElementById("read_more_text_rd");
var closeiconRD = document.getElementById("close_rd");
var openiconRD = document.getElementById("open_rd");
var toggleButtonSC = document.getElementById("readmore_sc");
var textSC = document.getElementById("read_more_text_sc");
var closeiconSC = document.getElementById("close_sc");
var openiconSC = document.getElementById("open_sc");
var toggleButtonMS = document.getElementById("readmore_ms");
var textMS = document.getElementById("read_more_text_ms");
var closeiconMS = document.getElementById("close_ms");
var openiconMS = document.getElementById("open_ms");
var toggleButtonPFP = document.getElementById("readmore_pfp");
var textPFP = document.getElementById("read_more_text_pfp");
var closeiconPFP = document.getElementById("close_pfp");
var openiconPFP = document.getElementById("open_pfp");
var toggleButtonJR = document.getElementById("readmore_jr");
var textJR = document.getElementById("read_more_text_jr");
var closeiconJR = document.getElementById("close_jr");
var openiconJR = document.getElementById("open_jr");
var toggleButtonKK = document.getElementById("readmore_kk");
var textKK = document.getElementById("read_more_text_kk");
var closeiconKK = document.getElementById("close_kk");
var openiconKK = document.getElementById("open_kk");
var toggleButtonSCF = document.getElementById("readmore_scf");
var textSCF = document.getElementById("read_more_text_scf");
var closeiconSCF = document.getElementById("close_scf");
var openiconSCF = document.getElementById("open_scf");
var toggleButtonAS = document.getElementById("readmore_as");
var textAS = document.getElementById("read_more_text_as");
var closeiconAS = document.getElementById("close_as");
var openiconAS = document.getElementById("open_as");
var toggleButtonSD = document.getElementById("readmore_sd");
var textSD = document.getElementById("read_more_text_sd");
var closeiconSD = document.getElementById("close_sd");
var openiconSD = document.getElementById("open_sd");

function showHide(text, icon1, icon2) {
  text.classList.toggle("hide");
  icon1.classList.toggle("hide");
  icon2.classList.toggle("hide");
}
toggleButtonCG.addEventListener("click", () => {
  showHide(textCG, closeiconCG, openiconCG);
});
toggleButtonAC.addEventListener("click", () => {
  showHide(textAC, closeiconAC, openiconAC);
});
toggleButtonRD.addEventListener("click", () => {
  showHide(textRD, closeiconRD, openiconRD);
});
toggleButtonSC.addEventListener("click", () => {
  showHide(textSC, closeiconSC, openiconSC);
});
toggleButtonMS.addEventListener("click", () => {
  showHide(textMS, closeiconMS, openiconMS);
});
toggleButtonPFP.addEventListener("click", () => {
  showHide(textPFP, closeiconPFP, openiconPFP);
});
toggleButtonJR.addEventListener("click", () => {
  showHide(textJR, closeiconJR, openiconJR);
});
toggleButtonAS.addEventListener("click", () => {
  showHide(textAS, closeiconAS, openiconAS);
});
toggleButtonSCF.addEventListener("click", () => {
  showHide(textSCF, closeiconSCF, openiconSCF);
});
toggleButtonKK.addEventListener("click", () => {
  showHide(textKK, closeiconKK, openiconKK);
});
toggleButtonSD.addEventListener("click", () => {
  showHide(textSD, closeiconSD, openiconSD);
});

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
    hamb.classList.remove("active");
    hamb1.classList.remove("colorMob");
    hamb3.classList.remove("colorMob");
  }
});
