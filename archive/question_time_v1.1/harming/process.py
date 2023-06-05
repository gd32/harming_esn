'''
Created on 2013/06/06

@author: Hirokazu Shirado

Edited 2017/4/28 by Chris German for Happiness Study

Edited 2023 by Hiroyasu Ando for TimePressure Study
'''

import csv
from datetime import datetime as dt
import Game
import networkx as nx
import sys
import matplotlib.pyplot as plt

def preProcess(filename):

    dataFile = csv.reader(open(filename, newline=''))

    # Skip the first row. 
    next(dataFile)

    gameId = filename.split("/")[-1]
    gameId = gameId.split(".")[0]

    IDlist = []
    Move = {}
    game = Game.Game(gameId)
    id = 0

    for data in dataFile:

        # Remove some spaces.
        data = list(map(lambda x:x.strip(), data))

        uid = int(data[0])
        event = data[1]

        # Process the format for datetime.
        #datetime = dt.strptime(data[2].replace("T"," ").replace("Z", ""), "%Y-%m-%d %H:%M:%S:%f")
        #datetime = dt.strptime(data[2].replace("T"," ").replace("Z", "").replace(",", ":"), "%Y-%m-%d %H:%M:%S:%f")
        datetime = dt.strptime(data[2].replace("T"," ").replace("Z", ""), "%Y-%m-%d %H:%M:%S")

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

        # Record parameters.
        if event == "initParameters":
            game.setDensity(float(d['connectivity']))
            game.setNumRounds(int(d['nRounds']))
            game.setRewiringRate(float(d['k']))
            game.setCooperationBalance(int(d['coc']), int(d['po']))
            game.setDisparityParameters(float(d['percentA']), int(d['scoreA']), int(d['scoreB']), d['showScore'])

            # Added
            #game.setTimePressure(d['questionTimeOn'], int(d['questionTime']))

            # Added
            game.setRewiringSatisfaction(d['happinessRewiring'])
    
    g = nx.Graph()
    lastRound = 0
    start = False

    for uid, d in sorted(Move.items()):

        event = d['event']
        time = d['time']

        if event == "initStart":
            game.setStartDate(time)
        
        elif event == "clientLogIn":
            game.newPlayer(d["clientId"], d["ipAddress"])
        
        elif event == "EndPractice2Start":
            start = True
        
        # Added
        elif event == "reportInitialSatisfaction" and start:
            #print(111)
            game.setPlayerInitialSatisfaction(int(d['round']), game.PID[d['pid']],d['satisfaction'])

        elif event == "InitialScore":
            game.setPlayerInitialScore(game.PID[d['pid']], int(d['score']))
            g.add_node(game.PID[d['pid']])
        
        elif event == "Connected":
            g.add_edge(game.PID[d['playerId1']], game.PID[d['playerId2']])
        
        elif event == "CooperationStart" and start:

            # Update Graph info before going to the next round. (see the next line.) 
            game.addGraph(lastRound, g.copy())

            # Go to the next round.
            lastRound += 1
            #game.setCooperationTime(lastRound, time)
        
        # Added
        # Record time to start the cooperation step. this is individual. 
        elif event == "StartMakingChoices" and start:
            game.setPlayerCooperationTime(lastRound, time, game.PID[d['pid']])

        
        # This measures pointless time. 
        elif event == "RewiringStart" and start:
            game.setRewiringTime(lastRound, time)
        
        elif event == "RewiringResultsStart" and start:
            newConnections = game.makeConnections(lastRound)
            for s in newConnections:
                pair = list(s)

                # Added
                # Check if players are on the graph. they might be dropped?
                if (pair[0] in g.nodes()) and (pair[1] in g.nodes()):
                    g.add_edge(pair[0], pair[1])
        
        elif event == "cooperationEvent" and start:
            if d['action'] == 'cooperate':
                game.setPlayerStrategy(int(d['round']), game.PID[d['pid']], 'C', time)
            #
            # Added
            #
            elif d['action'] == 'punish':
                #print(111)
                game.setPlayerStrategy(int(d['round']), game.PID[d['pid']], 'P', time)
            else:
                game.setPlayerStrategy(int(d['round']), game.PID[d['pid']], 'D', time)
        
        # Added
        elif event == "TimeUpDuringChoice" and start:
            print(int(d['round']))
            if d['action'] == 'punish':
                game.setPlayerTimeUpStrategy(int(d['round']), game.PID[d['pid']], 'P')
            elif d['action'] == 'cooperate':
                game.setPlayerTimeUpStrategy(int(d['round']), game.PID[d['pid']], 'C')
            else:
                game.setPlayerTimeUpStrategy(int(d['round']), game.PID[d['pid']], 'D')
        
        elif event == "reportSatisfaction" and start:
            game.setPlayerSatisfaction(int(d['round']), game.PID[d['pid']],d['satisfaction'])
        
        # Added
        elif event == "reportRewiringSatisfaction" and start:
            game.setPlayerRewiringSatisfaction(int(d['round']), game.PID[d['pid']],d['satisfaction'])
        
        # Added 2023/05/23
        elif event == "FinalScore":
            game.setPlayerFinalScore(game.PID[list(d.keys())[2]], int(d[list(d.keys())[2]]), time)

        elif event == "q1":
            #game.setQ1(game.PID[d['pid']], d['interested'], d['distressed'], d['excited'], d['upset'], d['strong'])
            game.setQ1(game.PID[d['pid']], d['interested'], d['distressed'], d['excited'], d['upset'], d['strong'], time)
        
        elif event == "q2":
            #game.setQ2(game.PID[d['pid']],d['guilty'],d['scared'],d['hostile'],d['enthusiastic'],d['proud'])
            game.setQ2(game.PID[d['pid']],d['guilty'],d['scared'],d['hostile'],d['enthusiastic'],d['proud'], time)
        
        elif event == "q3":
            #game.setQ3(game.PID[d['pid']],d['irritable'],d['alert'],d['ashamed'],d['inspired'],d['nervous'])
            game.setQ3(game.PID[d['pid']],d['irritable'],d['alert'],d['ashamed'],d['inspired'],d['nervous'], time)
        
        elif event == "q4":
            #game.setQ4(game.PID[d['pid']],d['determined'],d['attentive'],d['jittery'],d['active'],d['afraid'])
            game.setQ4(game.PID[d['pid']],d['determined'],d['attentive'],d['jittery'],d['active'],d['afraid'], time)
        
        elif event == "q5":
            #game.setGender(game.PID[d['pid']],d['gender'])
            game.setGender(game.PID[d['pid']],d['gender'], time)
        
        elif event == "q6":
            #game.setAge(game.PID[d['pid']],d['age'])
            game.setAge(game.PID[d['pid']],d['age'], time)
        
        # Add others and hispanic
        elif event == "q7":
            #game.setRace(game.PID[d['pid']],d['ethnicity'])
            game.setRace(game.PID[d['pid']],d['ethnicity'], time)
        
        # Added
        elif event == "q8":
            #game.setHispanic(game.PID[d['pid']],d['is_hispanic'])
            game.setHispanic(game.PID[d['pid']],d['is_hispanic'], time)
        
        elif event == "rewiringEvent" and start:
            if d['action'] == 'breakConnection':

                # Check if players are not dropped.
                if (g.has_edge(game.PID[d['pid']], game.PID[d['nid']])):
                    g.remove_edge(game.PID[d['pid']], game.PID[d['nid']])
                    #game.setPlayerBreakConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
                # Added
                game.setPlayerBreakConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
            elif d['action'] == 'makeConnection':
                game.setPlayerMakeConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
            # Added
            elif d['action'] == 'doNotBreakConnection':
                game.setPlayerNotBreakConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
            # Added
            elif d['action'] == 'doNotMakeConnection':
                game.setPlayerNotMakeConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)

            #elif d['action'] == 'maintainConnection':
                #game.setPlayerMaintainConnection(int(d['round']), game.PID[d['pid']], game.PID[d['nid']], time)
        
        elif event == "DropPlayer" and start:
            #print(111)
            # If players are dropped, they will be eliminated from the graph.
            if game.PID[d['pid']] in g:
                g.remove_node(game.PID[d['pid']]) 
    
    # Update the graph info for the last round.
    game.addGraph(lastRound, g.copy())

    return game

if __name__ == '__main__':

    argv = sys.argv

    if len(argv) < 2:
        print("Usage: # python %s filename" % argv[0])
        quit()
    
    input_file = argv[1]

    game = preProcess(input_file)

    game.writeJSON()



