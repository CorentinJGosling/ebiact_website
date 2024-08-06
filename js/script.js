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
