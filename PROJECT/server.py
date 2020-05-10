import asyncio
import async_timeout
import aiohttp
import json
import logging
import datetime
import socket
import ssl
import sys
import time

servers = ['Goloman', 'Hands', 'Holiday', 'Welsh', 'Wilkes']

ports = {
    'Goloman': 12040, 
    'Hands': 12041, 
    'Holiday': 12042, 
    'Welsh': 12043, 
    'Wilkes': 12044
}

communications = {
    'Goloman': ['Hands', 'Holiday', 'Wilkes'], 
    'Hands': ['Goloman', 'Wilkes'],
    'Holiday': ['Goloman', 'Welsh', 'Wilkes'], 
    'Welsh': ['Holiday'],
    'Wilkes': ['Goloman', 'Hands', 'Holiday'] 
}

API_KEY = "AIzaSyBhQuJ7wB6j3xB6Ly_TUEY9j8pOCAt0wfo"

client_devices = {}
tasks = {}

async def logWrite(text):
        if text == None:
                return
        try:
            log_file.write(text)
        except:
            pass

async def clientWrite(w, text):
        if text == None:
                return
        try:
            w.write(text.encode())
            await w.drain()
            w.write_eof()
        except:
            pass
        return

async def flooding(c):
    name = client_devices[c]
    comm = 'AT %s %s %s %s %s %s' % (c, name['server_name'], client_name['latitude'], client_name['longitude'],str(client_name['time_difference']), str(client_name['command_time']))
    
    for serv in communications[whichServer]:
        port = ports[serv]
        try:
            r, w = await asyncio.open_connection('127.0.0.1', port, loop=serv_loop)
            await logWrite('connected to ' + serv + '\n')
            await logWrite('propagated to %s TO %s:%s\n' % (c, serv, comm))
            await clientWrite(w, comm)
            await logWrite('closing connectiong with ' + serv + '\n\n')
        except:
            await logWrite('ERROR: propagating %s to %s\n\n' % (c,serv))


async def handleAT(w, cl, whichServer, latitude, longitude, diff, comm):
    if cl in client_devices and float(client_devices[cl]['command_time']) >= float(comm):
        await logWrite('command  sent for: ' + cl + '\n\n')
        return
    
    client_devices[cl] = {
        'server_name': whichServer,
        'latitude' : latitude,
        'longitude' : longitude,
        'time_difference' : float(diff),
        'command_time' : float(comm)
    }
    await flooding(cl)
                       

async def getResponse(session, url):
    async with async_timeout.timeout(10):
        async with session.get(url) as response:
            return await response.json()


async def handleWHATSAT(w, comm, time, cl, radius, resps):
    if cl not in client_devices:
        await logWrite('client is not in list: ' + cl + '\n')
        output = '? ' + comm
        await logWrite('sending: ' + output + '\n')
        await clientWrite(w,output)
        return None

    latude = client_devices[cl]["latitude"]
    longude = client_devices[cl]["longitude"]

    url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=%s,%s&radius=%d&key=%s'  % (latude, longude, float(radius), API_KEY)

    async with aiohttp.ClientSession() as session:
        await logWrite('Querying Google Places API for locations: (%s,%s) in radius: %s\n'
                       % (latude, longude, radius))

        response = await getResponse(session, url)
        res = response['results'][:int(resps)]
        dTime = client_devices[cl]["time_difference"]
        loc = get_lat + get_lon
        serverout = 'AT %s %s %s %s %s %s' %  (whichServer, str(dTime), cl, loc, time,
                                            json.dumps(res, indent=3))

        await logWrite('response to whatsat:\n' + serverout + '\n\n')

        await clientWrite(w,serverout)


async def handleIAMAT(w, timer, cl, loc, comm):
    delay = timer - float(comm)

    index = 0
    count = 0
    latude = ''
    longude = ''
    for x in loc:
        if x == '+' or x == '-':
            count = count + 1
            if count == 2:
                latude = loc[:(index-1)]
                longude = loc[index:]
        index = index + 1

    client_devices[cl] = {
        'server_name': whichServer,
        'latitude' : latude,
        'longitude' : longude,
        'time_difference' : float(delay),
        'command_time' : float(comm)
    }
    
    response = 'AT %s %s %s %s %s' %  (whichServer, str(delay), cl, loc, comm)
    await logWrite('response to iamat: \n' + response + '\n')
    await clientWrite(w, response)
    await flooding(cl)


async def handleCMD (w, whole, part):
    typec = part[0]
    timer = time.time()
    length = len(part)
    clist = ['IAMAT' , 'WHATSAT' , 'AT']
    if whole != '' and typec not in clist:
        await logWrite('invalid command response: \n' + '? ' + whole + '\n')
        put = '? ' + whole
        await clientWrite(w, put)
        return None
        
    elif typec == clist[1] and length < 4:
        await logWrite('invalid command response: \n' + '? ' + whole + '\n')
        put = '? ' + whole
        await clientWrite(w, put)
        return None

    if(len(part) > 3):
        await logWrite("command received: \n " + whole +  ' @ ' + str(timer) + '\n\n')
    
    if typec == clist[0]:
        await handleIAMAT(w, timer, part[1], part[2], part[3])        
    elif typec == clist[1]: 
        await handleWHATSAT(w, whole, timer, part[1], part[2], part[3]) 
    elif typec == clist[2]: 
        await handleAT(w, part[1], part[2], part[3], part[4], part[5], part[5])

async def getTask(r ,w):
    while not r.at_eof():
        cmd = await r.readline()
        part = cmd.decode().split(' ')
        await handleCMD(w, cmd.decode(), part)        


async def startHelper(r, w):
    atax = asyncio.create_task(getTask(r, w))
    tasks[atax] = (r, w)
    def closes(atax):
        log_file.write("closing  Connection...\n\n")
        del tasks[atax]
        w.close()

    atax.add_done_callback(closes)


if __name__ == '__main__':
    if(len(sys.argv) != 2):
        print('incorrect number of arguments!')
        exit(1)
    
    global whichServer
    whichServer = sys.argv[1]
    if not whichServer in servers:
        print('incorrect argument! The server is not valid! Needs to be one of the following: {}'.format(servers))
        exit(1)
    
    port_id = ports[whichServer]

    log = whichServer + ".log"
    global log_file
    open(log, "w").close()
    log_file = open(log, 'a+')
    log_file.write(whichServer + '\n')

    global looped 
    looped = asyncio.get_event_loop()
    coroutin = asyncio.start_server(startHelper, '127.0.0.1', port_id, loop=looped)
    server = looped.run_until_complete(coroutin)

    print('Serving on {}'.format(server.sockets[0].getsockname()))
    log_file.write('Serving on {}'.format(server.sockets[0].getsockname()) + '\n')

    try:
        looped.run_forever()
    except KeyboardInterrupt:
        pass

    server.close()
    looped.run_until_complete(server.wait_closed())
    looped.close()
