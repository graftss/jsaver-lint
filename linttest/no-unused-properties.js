
function makeColors() {
  return {
   red: 1,
   blue: 2,
   green: {
     ['dark??']: 3,
     light: 4,
     neon: 5,
   },
  }
}

function useColors(obj) {

    const x = obj.red + obj.blue;
    const y = obj.green.dark - obj.green.light

}

const x = makeColors()
useColors(x)
const y = makeColors()
const z = makeColors()