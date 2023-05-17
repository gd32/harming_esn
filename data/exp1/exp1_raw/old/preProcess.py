'''
Created on 2013/06/06

@author: Hirokazu Shirado

Edited 2017/4/28 by Chris German for Happiness Study
'''

def preProcess(filename):

    import csv
    import Game
    import networkx as nx
    from datetime import datetime as dt


    dataFile = csv.reader(open(filename, 'rU'))
    dataFile.next() # skip the header line

    gameId = filename.split("/")[-1]
    gameId = gameId.split(".")[0]


    IDlist = []
    Move = {}
    game = Game.Game(gameId)
    id = 0
    for data in dataFile:
        data = map(lambda x:x.strip(), data)

        uid = int(data[0])
        event = data[1]
        datetime = dt.strptime(data[2], "%Y-%m-%d %H:%M:%S,%f")
        dname = data[3]
        dvalue = data[4]


        if uid not in IDlist:
            IDlist.append(uid)
            id += 1
            Move[id] = {'event': event, 'time': datetime, dname: dvalue}
        else:
            Move[id][dname] = dvalue


    for uid, d in Move.items():
        event = d['event']
        if event == "initParameters":
            game.setDensity(float(d['connectivity']))
            game.setNumRounds(int(d['nRounds']))
            game.setRewiringRate(float(d['k']))
            game.setCooperationBalance(int(d['coc']), int(d['po']))
            game.setDisparityParameters(float(d['percentA']), int(d['scoreA']), int(d['scoreB']), d['showScore'])


    g = nx.Graph()
    lastRound = 0 #start from just connected graph (no C/D)
    start = False

    for uid, d in sorted(Move.items()):
        event = d['event']
        time = d['time'] #dt.strptime(d['time'], "%Y-%m-%d %H:%M:%S,%f")

        if event == "initStart":
            game.setStartDate(time)

        elif event == "clientLogIn": #just login players
            game.newPlayer(d["clientId"], d["ipAddress"])
            #game.setPlayerIPAddress(d["clientId"], d["ipAddress"])

        elif event == "EndPractice2Start":
            start = True

        elif event == "InitialScore":

            game.setPlayerInitialScore(game.PID[d['pid']], int(d['score'])) #actual players
            g.add_node(game.PID[d['pid']])

        elif event == "Connected":
            g.add_edge(game.PID[d['playerId1']], game.PID[d['playerId2']])

        elif event == "CooperationStart" and start:
            game.addGraph(lastRound, g.copy())
            lastRound += 1
            game.setCooperationTime(lastRound, time)

        elif event == "RewiringStart" and start:
            game.setRewiringTime(lastRound, time)

        elif event == "RewiringResultsStart" and start:
            newConnections = game.makeConnections(lastRound)
            for s in newConnections:
                pair = list(s)
                g.add_edge(pair[0], pair[1])


        elif event == "cooperationEvent" and start:
            if d['action'] == 'cooperate':
                game.setPlayerStrategy(int(d['round']), game.PID[d['pid']], 'C', time)
            else:
                game.setPlayerStrategy(int(d['round']), game.PID[d['pid']], 'D', time)

        elif event == "reportSatisfaction" and start:
            game.setPlayerSatisfaction(int(d['round']), game.PID[d['pid']],d['satisfaction'])


        elif event == "q1":
            game.setQ1(game.PID[d['pid']],d['interested'],d['distressed'],d['excited'],d['upset'],d['strong'])

        elif event == "q2":
            game.setQ2(game.PID[d['pid']],d['guilty'],d['scared'],d['hostile'],d['enthusiastic'],d['proud'])

        elif event == "q3":
            game.setQ3(game.PID[d['pid']],d['irritable'],d['alert'],d['ashamed'],d['inspired'],d['nervous'])

        elif event == "q4":
            game.setQ4(game.PID[d['pid']],d['determined'],d['attentive'],d['jittery'],d['active'],d['afraid'])

        elif event == "q5":
            game.setGender(game.PID[d['pid']],d['gender'])

        elif event == "q6":
            game.setAge(game.PID[d['pid']],d['age'])

        elif event == "q7":
            game.setRace(game.PID[d['pid']],d['ethnicity'])

        elif event == "rewiringEvent" and start:
            if d['action'] == 'breakConnection':
                if (g.has_edge(game.PID[d['pid']], game.PID[d['nid']])):
                    g.remove_edge(game.PID[d['pid']], game.PID[d['nid']])
                    game.setPlayerBreakConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
            elif d['action'] == 'makeConnection':
                game.setPlayerMakeConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
            elif d['action'] == 'maintainConnection':
                game.setPlayerMaintainConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)

        elif event == "DropPlayer" and start:
            if game.PID[d['pid']] in g:
                g.remove_node(game.PID[d['pid']])
            #game.setDropPlayers(int(d['curRound']), game.PID[d['pid']])
            
    game.addGraph(lastRound, g.copy())

    return game


if __name__ == '__main__':
    import sys
    import matplotlib.pyplot as plt

    argv = sys.argv

    if len(argv) < 2:
        print"Usage: # python %s filename" % argv[0]
        quit()
    input_file = argv[1]

    game = preProcess(input_file)

    game.writeJSON()
