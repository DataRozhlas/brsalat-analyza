// deno-lint-ignore-file no-explicit-any
import * as d3 from "npm:d3-dsv";

const rawData = await Deno.readTextFile("./nflvge4sbu5plimpb6gtpdk3ga.json");

const data = JSON.parse(rawData);

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

const IPs = result.map((item: any) => item.ip);
const uIPs: string[] = Array.from(new Set(IPs));

const completeIPs = uIPs.map((ip: string) => {
  return {
    originalIP: ip,
    completeIP: (ip.match(/\./g) || []).length === 2 ? `${ip}.255` : ip,
  };
});

const getLocation = async (ip: any) => {
  const response = await fetch(
    `http://ip-api.com/json/${ip}?fields=status,country,region,regionName,city,district,zip,isp,org,as,mobile,query`
  );
  const result = response.json();
  return result;
};

const sleep = (ms: number) => new Promise(r => setTimeout(r, ms));

//const sampleIP = completeIPs.slice(0, 100);

const geoResult = [];

for (let index = 0; index < completeIPs.length; index++) {
  await sleep(1333);
  const ipinfo: any = await getLocation(completeIPs[index].completeIP);
  geoResult.push({
    ip: completeIPs[index].originalIP,
    completeIP: completeIPs[index].completeIP,
    country: ipinfo.country,
    region: ipinfo.region,
    regionName: ipinfo.regionName,
    city: ipinfo.city,
    zip: ipinfo.zip,
  });
  console.log(
    index,
    completeIPs[index].completeIP,
    ipinfo.regionName,
    ipinfo.country
  );
}

// const geoResult = completeIPs.map(async ip => {
//   // deno-lint-ignore no-explicit-any
//   const ipinfo: any = await getLocation(ip.completeIP);
//   return {
//     ip: ip.originalIP,
//     completeIP: ip.completeIP,
//     region: ipinfo.region,
//     regionName: ipinfo.regionName,
//     city: ipinfo.city,
//     zip: ipinfo.zip,
//   };
// });

await Deno.writeTextFile("./geo.csv", d3.csvFormat(geoResult));
