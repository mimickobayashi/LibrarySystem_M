const btn = document.getElementById('btn')
const text = document.getElementsByClassName('text')
console.log(text)

btn.addEventListener('click', () => {
  text[0].classList.toggle('show')
})