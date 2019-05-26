// @ts-check

function effect(fn,count) {
  function call() {
    var args = Array.from(arguments)
    if(arguments.length - 1 === count) {
      args.pop()
      return fn.apply(null,args)
    } else {
      return function(n) {
        args.push(n)
        return call.apply(null,args)
      }
    }
  }
  return call
}
function GLEffect(name,count) {
  return function (gl) {
    const f = gl[name].bind(gl)
    return effect(f,count)
  }
}


exports.createWebGL = function(
  /** @type {HTMLCanvasElement} */
  canvas) {
  return function() {
    var ctx = canvas.getContext('webgl')
    return ctx
  }
}
exports.createProgramImp = function (left) {
  return function (right) {
    return function(
      /** @type {WebGLRenderingContext} */
      gl) {
      return function(
        /** @type {String} */
        vertex) {
          return function (fragment) {
        return function(
          ) {
          var vertShdr = gl.createShader(
            gl.VERTEX_SHADER
          )
          var fragShdr = gl.createShader(
            gl.FRAGMENT_SHADER
          )
          const program = gl.createProgram()
          if(vertShdr && fragShdr && program) {
            gl.shaderSource(vertShdr,vertex)
            gl.compileShader(vertShdr)
            if ( !gl.getShaderParameter(vertShdr, gl.COMPILE_STATUS) ) {
              var msg = "Vertex shader failed to compile.  The error log is:"
            + gl.getShaderInfoLog( vertShdr );
              return left(msg)
            }


            gl.shaderSource( fragShdr, fragment);
            gl.compileShader( fragShdr );
            if ( !gl.getShaderParameter(fragShdr, gl.COMPILE_STATUS) ) {
                var msg = "Fragment shader failed to compile.  The error log is:"
              + "<pre>" + gl.getShaderInfoLog( fragShdr ) + "</pre>";
                return left(msg)
            }


            gl.attachShader( program, vertShdr );
            gl.attachShader( program, fragShdr );
            gl.linkProgram( program );


            if ( !gl.getProgramParameter(program, gl.LINK_STATUS) ) {
                var msg = "Shader program failed to link.  The error log is:"
                    + "<pre>" + gl.getProgramInfoLog( program ) + "</pre>";
                return left(msg)
            }
            return right(program)
          }
          return left("Can't create program")
        }}
      }
    }
  }
}

exports.clearColor = function (r) {
  return function(g) {
    return function(b) {
      return function(a) {
        return function(gl) {
          return function () {
            gl.clearColor(r,g,b,a)
          }
        }
      }
    }
  }
}

exports.useProgram = function(gl){
  return function(p) {
    return function () {
      gl.useProgram(p)
    }
  }
}

exports.createBufferImpl = function (left) {
  return function (right) {
    return function (gl) {
      return function () {
        var buffer =gl.createBuffer()
        if(buffer) {
          return right(buffer)
        }
        return left("Failed to create buffer")
      }
    }
  }
}
exports.bindBuffer = GLEffect(
  "bindBuffer",
  2
)
exports.bufferData = function (
  /** @type {WebGLRenderingContext} */
  gl) {
  return function (type) {
    return function (arr) {
      return function (usage) {
        return function () {
          const data = new Float32Array(arr)
          gl.bufferData(
            type,
            data,
            usage
          )
          
        }
      }
    }
  }
}

exports.getAttribLocation = GLEffect(
  'getAttribLocation',2
)

// exports.getAttribLocation = function(
//   /** @type {WebGLRenderingContext} */
//   gl) {
//   return function (prog) {
//     return function (name) {
//       return function () {
//         return gl.getAttribLocation(prog,name)
//       }
//     }
//   }
// }

exports.vertexAttribPointer = GLEffect(
  'vertexAttribPointer',6
)
exports.enableVertexAttribArray = GLEffect(
  'enableVertexAttribArray',1
)
exports.clear = GLEffect (
  'clear',
  1
)
exports.drawArrays = GLEffect(
  'drawArrays',
  3
)
exports.orImpl = function(a) {
  return function(b) {
    return a | b
  }
}
exports.enable = GLEffect(
  'enable',
  1
)
// exports.clearColor = function() {}