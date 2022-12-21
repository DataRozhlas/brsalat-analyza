import * as d3 from "npm:d3-dsv";

// deno-lint-ignore no-explicit-any
const dictionary: any = {
  "Hlavni mesto Praha": "Hlavní město Praha",
  "Hlavní město Praha": "Hlavní město Praha",
  "Central Bohemia": "Středočeský kraj",
  "Jihocesky kraj": "Jihočeský kraj",
  "Plzensky kraj": "Plzeňský kraj",
  "Karlovarsky kraj": "Karlovarský kraj",
  "Ustecky kraj": "Ústecký kraj",
  "Liberecky kraj": "Liberecký kraj",
  "Kralovehradecky kraj": "Královéhradecký kraj",
  "Pardubicky kraj": "Pardubický kraj",
  "Kraj Vysocina": "Kraj Vysočina",
  "South Moravian": "Jihomoravský kraj",
  "Olomoucky kraj": "Olomoucký kraj",
  "Moravskoslezsky kraj": "Moravskoslezský kraj",
  Zlín: "Zlínský kraj",
};

const salatRaw = await Deno.readTextFile("./salat.csv");
const salat = d3.csvParse(salatRaw);

const geoRaw = await Deno.readTextFile("./geo.csv");
const geo = d3.csvParse(geoRaw);

const result = salat.map(
  (item: { ip: string; uid: string; loserID: string; winnerID: string }) => {
    const matched = geo.find((ip: { ip: string }) => ip.ip === item.ip);

    return {
      uid: +item.uid,
      loserID: +item.loserID,
      winnerID: +item.winnerID,
      country: matched.country,
      region: +matched.region,
      regionName: dictionary[matched.regionName] || matched.regionName,
      city: matched.city,
      zip: matched.zip,
    };
  }
);

console.log(
  new Set(
    result
      .filter((item: { country: string }) => item.country === "Czechia")
      .map((item: { regionName: string }) => item.regionName)
  )
);

console.log(new Set(result.map((item: { country: string }) => item.country)));

console.log(
  new Set(result.filter((item: { country: string }) => item.country === ""))
);

await Deno.writeTextFile("./geosalat.csv", d3.csvFormat(result));
