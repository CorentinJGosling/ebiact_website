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
  countUpTo(".vig1", 8);
  countUpTo(".vig2", 44);
  countUpTo(".vig3", 128);
  countUpTo(".vig4", 192);
};
ScrollReveal().reveal(".appear", { delay: 400 });
