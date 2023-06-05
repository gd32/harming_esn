
'''
Created on Mar 21, 2012

@author: Hirokazu Shirado

Editied on 2022 by Hiroyasu Ando
'''

import Player
import json
from networkx.readwrite import json_graph
from datetime import datetime as dt


class Game:

    def __init__(self, gameId):
        self.gameId = gameId
        self.Graphs = {}
        self.LoginPlayers = {}
        self.Players = {}
        self.Drops = {}
        self.MakeCandidates = {}
        self.CooperationTime = {}
        self.RewiringTime = {}
        self.numPID = 0
        self.PID = {}
    
    def setDensity(self, density):
        self.density = density
    
    # Added
    def setTimePressure(self, timePressure, timeLimit):
        self.timePressure = timePressure

        if self.timePressure == 'true':
            self.timeLimit = timeLimit
        else:
            self.timeLimit = ''
            
    # Added 
    def setRewiringSatisfaction(self, rewiringSatisfaction):
        self.rewiringSatisfaction = rewiringSatisfaction
        #print(self.rewiringSatisfaction)

    def setNumRounds(self, numRounds):
        self.numRounds = numRounds
    
    def setRewiringRate(self, rewiringRate):
        self.rewiringRate = rewiringRate
    
    def setCooperationBalance(self, cost, profit):
        self.cost = cost
        self.profit = profit
    
    def setDisparityParameters(self, percentA, scoreA, scoreB, showScore):
        self.percentA = percentA
        self.scoreA = scoreA
        self.scoreB = scoreB
        self.showScore = showScore
    
    def setStartDate(self, date):
        self.startDate = date

    def newPlayer(self, playerId, ipAddress):
        if playerId not in self.PID.keys():
            self.PID[playerId] = "p" + str(self.numPID)
            self.LoginPlayers[self.PID[playerId]] = Player.Player(self.PID[playerId], self.gameId, self.cost, self.profit, ipAddress, False)
            self.numPID = self.numPID + 1
    
    def setPlayerInitialScore(self, playerId, score):
        self.Players[playerId] = self.LoginPlayers[playerId]
        self.Players[playerId].setInitialScore(score)
    
    def addGraph(self, round, graph):

        self.Graphs[round] = graph

        # Added
        # graph.nodes()
        # If round is 0, cooperation step has not been conducted yet.
        if round == 0:
            cooperators = []
            punishers = []
        else:
            # Find out cooperators from the previous network, since current network will eliminate some droopped players.
            cooperators = [x for x in self.Graphs[round-1].nodes() if x in self.Players and self.Players[x].getStrategy(round) == 'C']
            
            # Added
            punishers = [x for x in self.Graphs[round-1].nodes() if x in self.Players and self.Players[x].getStrategy(round) == 'P']

        for playerId in graph.nodes():

            neighbors = graph.neighbors(playerId)

            # Added
            neighbors = [x for x in neighbors]

            if playerId in self.Players:
                self.Players[playerId].setNeighbors(round, neighbors)
                self.Players[playerId].setCooperators(round, cooperators)

                # Added
                self.Players[playerId].setPunishers(round, punishers)

                self.Players[playerId].setPayoff(round)
    
    #def setCooperationTime(self, round, time):
        #self.CooperationTime[round] = time
    
    # Added
    def setPlayerCooperationTime(self, round, time, playerId):
        self.Players[playerId].setCooperationTime(round, time)

    def setRewiringTime(self, round, time):
        self.RewiringTime[round] = time
    
    # Find out agree and agree connections. 
    def makeConnections(self, round):
        newConnections = []
        for p, C in self.MakeCandidates.items():
            for c in C:
                if c in self.MakeCandidates and p in self.MakeCandidates[c]:
                    if set([p, c]) not in newConnections:
                        newConnections.append(set([p, c]))
                        self.Players[p].setMakeLink(round, c)
                        self.Players[c].setMakeLink(round, p)
        self.MakeCandidates = {}
        return newConnections
    
    def setPlayerStrategy(self, round, playerId, strategy, time, practice=False):
        
        # I do not think it is needed.
        if playerId not in self.Players:
            self.Players[playerId] = self.LoginPlayers[playerId]
            if practice:
                self.Players[playerId].setInitialScore(self.practiceScore)
                for i in range(1, round):
                    print("Players joined during practice rounds. Something is wrong with data")
                    self.Players[playerId].TotalPayoff[i] = self.practiceScore
            else:
                self.Players[playerId].setInitialScore(self.scoreA)
                for i in range(1, round):
                    print("Players joined during actual rounds. Something is wrong with data")
                    self.Players[playerId].TotalPayoff[i] = self.scoreA

        if isinstance(time, int):
            # I do not think it is needed. I mean this IF part. (ElSE is necessary.)
            print("Time is int. Something is wrong with data")
            self.Players[playerId].setStrategy(round, strategy, "")
        else:
            #self.Players[playerId].setStrategy(round, strategy, time - self.CooperationTime[round])
            # Added
            self.Players[playerId].setStrategy(round, strategy, time)
    
    # Added
    def setPlayerTimeUpStrategy(self, round, playerId, strategy):
        self.Players[playerId].setTimeUpStrategy(round, strategy)

    def setPlayerSatisfaction(self, round, playerId, satisfaction, practice=False):
        self.Players[playerId].setSatisfaction(round, satisfaction)
    
    # Added
    def setPlayerRewiringSatisfaction(self, round, playerId, satisfaction, practice=False):
        self.Players[playerId].setRewiringSatisfaction(round, satisfaction)
    
    # Added
    def setPlayerInitialSatisfaction(self, round, playerId, satisfaction, practice=False):
        self.Players[playerId].setInitialSatisfaction(round, satisfaction)
    
    def setQ1(self, playerId, interested, distressed, excited, upset, strong, time):
        self.Players[playerId].setQ1(interested, distressed, excited, upset, strong, time)
    
    def setQ2(self, playerId, guilty, scared, hostile, enthusiastic, proud, time):
        self.Players[playerId].setQ2(guilty, scared, hostile, enthusiastic, proud, time)
    
    def setQ3(self, playerId, irritable, alert, ashamed, inspired, nervous, time):
        self.Players[playerId].setQ3(irritable, alert, ashamed, inspired, nervous, time)
    
    def setQ4(self, playerId, determined, attentive, jittery, active, afraid, time):
        self.Players[playerId].setQ4(determined, attentive, jittery, active, afraid, time)
    
    def setGender(self,playerId,gender, time):
        self.Players[playerId].setGender(gender, time)
    
    def setAge(self,playerId,age, time):
        self.Players[playerId].setAge(age, time)
    
    def setRace(self,playerId,race, time):  
        self.Players[playerId].setRace(race, time)

    # Added
    def setHispanic(self,playerId,hisp, time):
        self.Players[playerId].setHispanic(hisp, time)
    
    def setPlayerBreakConnection(self, round, playerId, targetId, time):

        # I do not think this is needed. (IF part)
        if (playerId) not in self.Players: return
        self.Players[playerId].setBreakLink(round, targetId, time - self.RewiringTime[round])
        self.Players[targetId].setBeBrokenLink(round, playerId)
    
    def setPlayerMakeConnection(self, round, playerId, targetId, time):

        # I do not think this is needed. 
        if playerId not in self.Players: return

        # This is needed.
        if playerId not in self.MakeCandidates:
            self.MakeCandidates[playerId] = []
        self.MakeCandidates[playerId].append(targetId)
        self.Players[playerId].setTryMakeLink(round, targetId, time - self.RewiringTime[round])
    
    #def setPlayerMaintainConnection(self, round, playerId, targetId, time):
        #if playerId not in self.Players: return
        #if self.Graphs[round - 1].has_edge(playerId, targetId):
            #self.Players[playerId].setNotBreakLink(round, targetId, time - self.RewiringTime[round])
        #else:
            #self.Players[playerId].setNotMakeLink(round, targetId, time - self.RewiringTime[round])
    
    # Added
    def setPlayerNotBreakConnection(self, round, playerId, targetId, time):
        self.Players[playerId].setNotBreakLink(round, targetId, time - self.RewiringTime[round])
    
    # Added
    def setPlayerNotMakeConnection(self, round, playerId, targetId, time):
        self.Players[playerId].setNotMakeLink(round, targetId, time - self.RewiringTime[round])
    
    # Record all data in the graph. But some droopped players won't be recorded. 
    def writeJSON(self, folder = ""):

        Anony = {}
        num = 0

        for playerId in self.Players.keys():
            Anony[playerId] = num
            num = num + 1
        
        DG = {}

        for round in range(self.numRounds + 1):
            graph = self.Graphs[round]
            graph.graph["round"] = round

            print("------------------------")

            for n in graph.nodes():
                #print('---------------------')
                #print(n)
                graph.nodes[n]["initScore"] = self.Players[n].initialScore
                graph.nodes[n]["ipAddress"] = self.Players[n].address
                #print(graph.nodes[n]["ipAddress"])

                graph.nodes[n]["gender"] = self.Players[n].Gender
                #Added
                graph.nodes[n]["gender_time"] = self.Players[n].gender_time

                graph.nodes[n]["age"] = self.Players[n].Age
                #Added
                graph.nodes[n]["age_time"] = self.Players[n].age_time
                # Added
                graph.nodes[n]["initialSatisfaction"] = self.Players[n].InitialSatisfaction
                #print(graph.nodes[n]["initialSatisfaction"])

                # Added
                graph.nodes[n]["is_hispanic"] = self.Players[n].hisp
                #print(graph.nodes[n]["is_hispanic"])
                #Added
                graph.nodes[n]["hisp_time"] = self.Players[n].hisp_time

                # others
                graph.nodes[n]["race"] = self.Players[n].Race
                #print(graph.nodes[n]["race"])
                #Added
                graph.nodes[n]["race_time"] = self.Players[n].race_time

                graph.nodes[n]["interested"] = self.Players[n].interested
                graph.nodes[n]["distressed"] = self.Players[n].distressed
                graph.nodes[n]["excited"] = self.Players[n].excited
                graph.nodes[n]["upset"] = self.Players[n].upset
                graph.nodes[n]["strong"] = self.Players[n].strong
                #Added
                graph.nodes[n]["q1_time"] = self.Players[n].q1_time


                graph.nodes[n]["guilty"] = self.Players[n].guilty
                graph.nodes[n]["scared"] = self.Players[n].scared
                graph.nodes[n]["hostile"] = self.Players[n].hostile
                graph.nodes[n]["enthusiastic"] = self.Players[n].enthusiastic
                graph.nodes[n]["proud"] = self.Players[n].proud
                #Added
                graph.nodes[n]["q2_time"] = self.Players[n].q2_time


                graph.nodes[n]["irritable"] = self.Players[n].irritable
                graph.nodes[n]["alert"] = self.Players[n].alert
                graph.nodes[n]["ashamed"] = self.Players[n].ashamed
                graph.nodes[n]["inspired"] = self.Players[n].inspired
                graph.nodes[n]["nervous"] = self.Players[n].nervous
                #Added
                graph.nodes[n]["q3_time"] = self.Players[n].q3_time

                graph.nodes[n]["determined"] = self.Players[n].determined
                graph.nodes[n]["attentive"] = self.Players[n].attentive
                graph.nodes[n]["jittery"] = self.Players[n].jittery
                graph.nodes[n]["active"] = self.Players[n].active
                graph.nodes[n]["afraid"] = self.Players[n].afraid
                #Added
                graph.nodes[n]["q4_time"] = self.Players[n].q4_time

                #######################################################
                #print(graph.nodes[n]["q1_time"])
                #print(graph.nodes[n]["q2_time"])
                #print(graph.nodes[n]["q3_time"])
                #print(graph.nodes[n]["q4_time"])
                #print(graph.nodes[n]["gender_time"])
                #print(graph.nodes[n]["age_time"])
                #print(graph.nodes[n]["race_time"])
                #print(graph.nodes[n]["hisp_time"])
                #######################################################

                if round > 0:
                    graph.nodes[n]["payoff"] = self.Players[n].Payoff[round]
                    graph.nodes[n]["cumulativePayoff"] = self.Players[n].TotalPayoff[round]
                    print(graph.nodes[n]["cumulativePayoff"])

                    if  round not in self.Players[n].Strategy:
                        graph.nodes[n]["behavior"] =""
                        graph.nodes[n]["behaviorTime"] = ""
                    else:
                        graph.nodes[n]["behavior"] = self.Players[n].Strategy[round]

                        if round not in self.Players[n].StrategyTime:

                            # Added
                            graph.nodes[n]["behaviorTime"] = ""
                        else:
                            graph.nodes[n]["behaviorTime"] = self.Players[n].StrategyTime[round].seconds * 1000 + (self.Players[n].StrategyTime[round].microseconds)/1000
                    
                    #print(graph.nodes[n]["behavior"])
                    #print(graph.nodes[n]["behaviorTime"])

                    if round not in self.Players[n].Satisfaction:
                        graph.nodes[n]["satisfaction"] = ""
                    else:
                        # maybe
                        graph.nodes[n]["satisfaction"] = self.Players[n].Satisfaction[round]
                    
                    graph.nodes[n]["makeLink"] = self.getLinkTargets(self.Players[n].TryMakeLink, round)
                    graph.nodes[n]["notMakeLink"] = self.getLinkTargets(self.Players[n].NotMakeLink, round)
                    graph.nodes[n]["breakLink"] = self.getLinkTargets(self.Players[n].BreakLink, round)
                    graph.nodes[n]["notBreakLink"] = self.getLinkTargets(self.Players[n].NotBreakLink, round)

                    # Added
                    if round not in self.Players[n].timeUpRound:
                        graph.nodes[n]["timeUp"] = False
                    else:
                        graph.nodes[n]["timeUp"] = True
                    
                    #print(graph.nodes[n]["timeUp"])
                    
                    # Added
                    # Check
                    if round not in self.Players[n].RewiringSatisfaction:
                        graph.nodes[n]["rewiringSatisfaction"] = ""
                    else:
                        graph.nodes[n]["rewiringSatisfaction"] = self.Players[n].RewiringSatisfaction[round]
                    
                    #print(graph.nodes[n]["rewiringSatisfaction"])

            for e in graph.edges():
                graph.edges[(e[0],e[1])]['id'] = e

            roundName = round
            DG[roundName] = json_graph.node_link_data(graph)
        
        Game = {}
        Game["gameId"] = self.gameId
        Game["cost"] = self.cost
        Game["profit"] = self.profit
        Game["percentA"] = self.percentA
        Game["scoreA"] = self.scoreA
        Game["scoreB"] = self.scoreB
        Game["initDensity"] = self.density
        Game["rewiringRate"] = self.rewiringRate
        Game["showScore"] = self.showScore

        #Added
        #Game["timePressure"] = self.timePressure
        #Game["timeLimit"] = self.timeLimit

        # Added
        Game["happinessRewiring"] = self.rewiringSatisfaction

        Game["result"] = DG

        json.dump(Game, open(self.gameId + '.json', 'w'))

    def getLinkTargets(self, dict, round):
        if round in dict:
            return dict[round]
        else:
            return []
    
    












                


        



    



























    


    

    

