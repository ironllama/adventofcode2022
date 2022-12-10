const tailVisited = [];
let allTails = new Array(10).fill(0);
allTails = allTails.map(tail => ({ x: 0, y: 0 }));

require('fs').readFileSync('09.dat', 'utf-8').split("\n").forEach(line => {
// [
// "R 5",
// "U 8",
// "L 8",
// "D 3",
// "R 17",
// "D 10",
// "L 25",
// "U 20",
// ].forEach(line => {
  [dir, amt] = line.split(" ");
  // console.log("DIR: ", dir, "AMT:", amt);

  for (let movement_idx = 0; movement_idx < amt; movement_idx++) {
    for (let idx = 0; idx < allTails.length - 1; idx++) {
      let curr_head = allTails[idx];

      if (idx === 0) {
        if (dir === "U") curr_head.y += 1;
        else if (dir === "D") curr_head.y -= 1;
        else if (dir === "R") curr_head.x += 1;
        else if (dir === "L") curr_head.x -= 1;
      }

      let curr_tail = allTails[idx + 1];
      allTails[idx + 1] = follow_head(curr_head, curr_tail, idx);
      // display_state(allTails);
    }
    // display_state(allTails);
  }
});

console.log("FINAL: ", tailVisited.length + 1);

function follow_head(curr_head, curr_tail, allTailIdx) {
  if (curr_head.x > (curr_tail.x + 1)) {
    curr_tail.x += 1;
    if (curr_head.y > curr_tail.y) curr_tail.y += 1;
    if (curr_head.y < curr_tail.y) curr_tail.y -= 1;
    if (allTailIdx === 8) add_visited(curr_tail);
  }
  if (curr_head.x < (curr_tail.x - 1)) {
    curr_tail.x -= 1;
    if (curr_head.y > curr_tail.y) curr_tail.y += 1;
    if (curr_head.y < curr_tail.y) curr_tail.y -= 1;
    if (allTailIdx === 8) add_visited(curr_tail);
  }

  if (curr_head.y > (curr_tail.y + 1)) {
    curr_tail.y += 1;
    if (curr_head.x > curr_tail.x) curr_tail.x += 1;
    if (curr_head.x < curr_tail.x) curr_tail.x -= 1;
    if (allTailIdx === 8) add_visited(curr_tail);
  }
  if (curr_head.y < (curr_tail.y - 1)) {
    curr_tail.y -= 1;
    if (curr_head.x > curr_tail.x) curr_tail.x += 1;
    if (curr_head.x < curr_tail.x) curr_tail.x -= 1;
    if (allTailIdx === 8) add_visited(curr_tail);
  }

  return curr_tail;
}

function add_visited(curr_tail) {
  let tail_visit_found = tailVisited.find(point => point.x === curr_tail.x && point.y === curr_tail.y);
  if (typeof tail_visit_found === 'undefined') tailVisited.push({ x: curr_tail.x, y: curr_tail.y});
}

function display_state(tails) {
  // const highestX = tails.reduce((acc, tail) => tail.x > acc ? tail.x : acc, 0);
  // const lowestX = tails.reduce((acc, tail) => tail.x < acc ? tail.x : acc, 0);
  // const highestY = tails.reduce((acc, tail) => tail.y > acc ? tail.y : acc, 0);
  // const lowestY = tails.reduce((acc, tail) => tail.y < acc ? tail.y : acc, 0);

  // for (let curr_y = highestY; curr_y >= lowestY; curr_y--) {
  //   let line = "";
  //   for (let curr_x = lowestX; curr_x <= highestX; curr_x++) {
  //     let tail_found = false;
  //     for (let i = 0; i < tails.length; i++) {
  //       if (tails[i].x == curr_x && tails[i].y == curr_y) {
  //         line += i;
  //         tail_found = true;
  //         break;
  //       }
  //     }
  //     if (!tail_found) line += "."
  //   }
  //   console.log(line);
  // }
  // console.log();

  for (let curr_y = 15; curr_y >= 0; curr_y--) {
    let line = "";
    for (let curr_x = 0; curr_x <= 15; curr_x++) {
      let tail_found = false;
      for (let i = 0; i < tails.length; i++) {
        // console.log("COMPARE: ", tails[i], curr_x, curr_y);
        if (tails[i].x == curr_x && tails[i].y == curr_y) {
          line += i;
          tail_found = true;
          break;
        }
      }
      if (!tail_found) line += "."
    }
    console.log(line);
  }
  console.log();
}