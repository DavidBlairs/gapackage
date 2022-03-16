$(document).on("shiny:connected", function() {
  var canvas = document.getElementById('matterjs-canvas');

  var width = 800,
      height = 800;

  canvas.width = width;
  canvas.height = height;

  // module aliases
  var Engine = Matter.Engine,
      Render = Matter.Render,
      Runner = Matter.Runner,
      Body = Matter.Body,
      Events = Matter.Events,
      Composites = Matter.Composites,
      Common = Matter.Common,
      Constraint = Matter.Constraint,
      MouseConstraint = Matter.MouseConstraint,
      Mouse = Matter.Mouse,
      Composite = Matter.Composite,
      Bodies = Matter.Bodies,
      World = Matter.World;

  // create an engine
  var engine = Engine.create(canvas);

  // create a renderer
  var render = Render.create({
      canvas: canvas,
      engine: engine,
      width: 800,
      height: 800
  });

  // run the renderer
  Render.run(render);

  engine.positionIterations = 100000;
  engine.velocityIterations = 100000;

  // run the engine
  setInterval(function() { Engine.update(engine, 1000 * 5 / 60); }, (1000 / 60));

  var group = Body.nextGroup(true);

  // add mouse control
  var mouse = Mouse.create(render.canvas),
      mouseConstraint = MouseConstraint.create(engine, {
          mouse: mouse,
          constraint: {
              stiffness: 0.1,
              render: {
                  visible: false
              }
          }
      });

  Composite.add(engine.world, mouseConstraint);

  right_segment = Bodies.rectangle(755, 0, 1, 1600, {
      collisionFilter: { group: group },
      isStatic: true,
      label: "right_boundary"
    }
  );

  Composite.add(engine.world, right_segment);

  Shiny.addCustomMessageHandler("curve_solid", function(points){
    // variables to save all calculated values
    let n = points.length;

    coordinates = [];

    // calculate x and y coordinates of all the points
    for (let i = 1; i <= n; i++){
      // calculate x / y
      let x = 50 + ((i - 1) * (700 / (n - 1))),
          y = 525 - points[i - 1];

      coordinates[i - 1] = {x: x, y: y};
    }
    coordinates[n] = {x: coordinates[n - 1]["x"] + 300, y: 800};
    coordinates[n + 1] = {x: coordinates[0]["x"] - 300, y: 800};

    console.log(coordinates)

    terrain = Bodies.fromVertices(400, 400, coordinates, {
      isStatic: true
    });

    Composite.add(engine.world, terrain);
  })


  // set simulation speed
  Shiny.addCustomMessageHandler("set_speed", function(speed){
    engine.timing.timeScale = speed[0];
  })

  // add a ball
  Shiny.addCustomMessageHandler("add_ball", function(arg){
    var group = Body.nextGroup(true);

    ball = Bodies.circle(75, -100, 25, {
      collisionFilter: { group: group }
    });
    Composite.add(engine.world, [ball])
  });

  // download text event
  function download(filename, text) {
    var pom = document.createElement('a');
    pom.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
    pom.setAttribute('download', filename);

    if (document.createEvent) {
        var event = document.createEvent('MouseEvents');
        event.initEvent('click', true, true);
        pom.dispatchEvent(event);
    }
    else {
        pom.click();
    }
  }

  // an example of using collisionStart event on an engine
  Events.on(engine, 'collisionStart', (event) => {
    event.pairs.forEach((collision) => {
      if (collision.bodyA.label === "right_boundary" || collision.bodyB.label === "right_boundary"){
        download("simulation_end.txt", "complete;")
      }
    });
  });

  // clear screen
  Shiny.addCustomMessageHandler("clear_screen", function(arg){
    Composite.clear(engine.world);


    right_segment = Bodies.rectangle(755, 0, 1, 1600, {
        collisionFilter: { group: group },
        isStatic: true,
        label: "right_boundary"
      }
    );

    Composite.add(engine.world, right_segment);
  })

  // add curve
  Shiny.addCustomMessageHandler("add_curve", function(points){
    // variables to save all calculated values
    let x_values      = [],
        y_values      = [],
        length_values = [],
        theta_values  = [];

    let n = points.length;

    // calculate x and y coordinates of all the points
    for (let i = 1; i <= n; i++){
      // calculate x / y
      let x = 50 + ((i - 1) * (700 / (n - 1))),
          y = 525 - points[i - 1];

      // populate arrays
      x_values[i - 1] = x,
      y_values[i - 1] = y;
    }

    // calculate segment length and rotational component
    for (let i = 1; i <= n - 1; i++){
      // get points on either side of the segment
      let first_point  = [x_values[i - 1], y_values[i - 1]],
          second_point = [x_values[i]    , y_values[i]    ];

      // calculate x and y difference between points
      let x_diff = second_point[0] - first_point[0],
          y_diff = second_point[1] - first_point[1];

      // calculate segment length
      length_values[i - 1] = Math.sqrt(Math.pow(x_diff, 2) + Math.pow(y_diff, 2));

      // calculate rotational component
      if (y_diff === 0){
        theta_values[i - 1] = 0;
      } else {
        theta_values[i - 1] = Math.atan(y_diff / x_diff);
      }
    }

    var group = Body.nextGroup(true);

    // construct body
    for (let i = 0; i < n - 1; i++){
      current_segment = Bodies.rectangle(
        (x_values[i] + x_values[i + 1]) / 2,
        (y_values[i] + y_values[i + 1]) / 2,
        length_values[i] + 10, 50, {
          collisionFilter: { group: group },
          isStatic: true,
          angle: theta_values[i]
        }
      );
      Composite.add(engine.world, current_segment);
    }

  })
});

