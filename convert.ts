import * as d3 from "npm:d3-dsv";

const rawData = await Deno.readTextFile("./nflvge4sbu5plimpb6gtpdk3ga.json");

const data = JSON.parse(rawData);

// deno-lint-ignore no-explicit-any
const result = data.map((item: any) => {
  return {
    uid: +item.Item.uid.N,
    //   client: item.Item.client.S,
    loserID: +item.Item.loserID.N,
    winnerID: +item.Item.winnerID.N,
    ip: item.Item.ip.S,
    //   ref: item.Item.url.S,
  };
});

await Deno.writeTextFile("./salat.csv", d3.csvFormat(result));

const codeBook = JSON.parse(await Deno.readTextFile("./salaty.json"));

// deno-lint-ignore no-explicit-any
const codeBookResult = codeBook.map((item: any) => {
  return { id: +item.id, item: item.name };
});

await Deno.writeTextFile("./codebook.csv", d3.csvFormat(codeBookResult));
