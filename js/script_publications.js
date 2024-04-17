const deployPub = document.getElementById("publiBTN");
const textDeploy = document.querySelector(".publiTXT");

deployPub.addEventListener("click", () => {
  textDeploy.classList.toggle("show");
});
