
'''
Created on Mar 21, 2012

@author: Hirokazu Shirado
'''

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


    def setId(self, gameId):
        self.gameId = gameId

    def setStartDate(self, date):
        self.startDate = date

    def setRewiringRate(self, rewiringRate):
        self.rewiringRate = rewiringRate

    def setNumRounds(self, numRounds):
        self.numRounds = numRounds

    def setCooperationBalance(self, cost, profit):
        self.cost = cost
        self.profit = profit

    def setDisparityParameters(self, percentA, scoreA, scoreB, showScore):
        self.percentA = percentA
        self.scoreA = scoreA
        self.scoreB = scoreB
        self.showScore = showScore

    def setPracticeParameters(self, practiceScore, practiceRounds):
        self.practiceScore = practiceScore
        self.numPractice = practiceRounds

    def setDensity(self, density):
        self.density = density

    def setCooperationTime(self, round, time):
        self.CooperationTime[round] = time

    def setRewiringTime(self, round, time):
        self.RewiringTime[round] = time


    def addGraph(self, round, graph):
        self.Graphs[round] = graph

        cooperators = [x for x in graph.nodes() if self.Players.has_key(x) and self.Players[x].getStrategy(round) == 'C']
        for playerId in graph.nodes():
            neighbors = graph.neighbors(playerId)
            #c_neighbors = [x for x in neighbors if self.Players.has_key(x) and self.Players[x].getStrategy(round) == 'C']

            if self.Players.has_key(playerId):
                self.Players[playerId].setNeighbors(round, neighbors)
                #self.Players[playerId].setCooperativeNeighbors(round, c_neighbors)
                self.Players[playerId].setCooperators(round, cooperators)
                self.Players[playerId].setPayoff(round) #based on the previous condition


    def newPlayer(self, playerId, ipAddress):
        import Player
        if playerId not in self.PID.keys():
            self.PID[playerId] = "p" + str(self.numPID)
            self.LoginPlayers[self.PID[playerId]] = Player.Player(self.PID[playerId], self.gameId, self.cost, self.profit, ipAddress, False)
            self.numPID = self.numPID + 1

    def newAI(self, playerId, aiNum):
        import Player
        if playerId in self.PID.keys():
            pID = self.PID[playerId]
            self.PID[playerId + "_" + aiNum] = pID + "_" + aiNum
            self.LoginPlayers[pID + "_" + aiNum] = Player.Player(self.PID[playerId + "_" + aiNum], self.gameId, self.cost, self.profit, "", True)


    def setPlayerIPAddress(self, playerId, address):
        self.LoginPlayers[playerId].setIPAddress(address)

    def setPlayerInitialScore(self, playerId, score):
        self.Players[playerId] = self.LoginPlayers[playerId]; #Actual participants
        self.Players[playerId].setInitialScore(score)


    def setPlayerStrategy(self, round, playerId, strategy, time, practice=False):
        if not self.Players.has_key(playerId):
            #self.newPlayer(playerId)
            self.Players[playerId] = self.LoginPlayers[playerId];
            if practice:
                self.Players[playerId].setInitialScore(self.practiceScore)
                for i in range(1, round):
                    self.Players[playerId].TotalPayoff[i] = self.practiceScore
            else:
                self.Players[playerId].setInitialScore(self.scoreA)
                for i in range(1, round):
                    self.Players[playerId].TotalPayoff[i] = self.scoreA
            #return
        if isinstance(time, int):
            self.Players[playerId].setStrategy(round, strategy, "")
        else:
            self.Players[playerId].setStrategy(round, strategy, time - self.CooperationTime[round])


    #Added for happiness study
    def setPlayerSatisfaction(self,round,playerId,satisfaction,practice=False):
        self.Players[playerId].setSatisfaction(round,satisfaction)

    def setGender(self,playerId,gender):
        self.Players[playerId].setGender(gender)

    def setAge(self,playerId,age):
        self.Players[playerId].setAge(age)

    def setIs_Hispanic(self,playerId,hispanic):
        self.Players[playerId].setIs_Hispanic(hispanic)

    def setRace(self,playerId,race):  #check may use ethnicity
        self.Players[playerId].setRace(race)

    def setQ1(self, playerId, interested, distressed, excited, upset, strong):
        self.Players[playerId].setQ1(interested, distressed, excited, upset, strong)

    def setQ2(self, playerId, guilty, scared, hostile, enthusiastic, proud):
        self.Players[playerId].setQ2(guilty, scared, hostile, enthusiastic, proud)

    def setQ3(self, playerId, irritable, alert, ashamed, inspired, nervous):
        self.Players[playerId].setQ3(irritable, alert, ashamed, inspired, nervous)

    def setQ4(self, playerId, determined, attentive, jittery, active, afraid):
        self.Players[playerId].setQ4(determined, attentive, jittery, active, afraid)

    def makeConnections(self, round):
        newConnections = []
        for p, C in self.MakeCandidates.items():
            for c in C:
                if self.MakeCandidates.has_key(c) and p in self.MakeCandidates[c]:
                    if set([p, c]) not in newConnections:
                        newConnections.append(set([p, c]))
                        self.Players[p].setMakeLink(round, c)
                        self.Players[c].setMakeLink(round, p)
        self.MakeCandidates = {}
        return newConnections

    def setPlayerBreakConnection(self, round, playerId, targetId, time):
        if not self.Players.has_key(playerId): return
        self.Players[playerId].setBreakLink(round, targetId, time - self.RewiringTime[round])
        self.Players[targetId].setBeBrokenLink(round, playerId)

    def setPlayerMakeConnection(self, round, playerId, targetId, time):
        if not self.Players.has_key(playerId): return
        if not self.MakeCandidates.has_key(playerId):
            self.MakeCandidates[playerId] = []
        self.MakeCandidates[playerId].append(targetId)
        self.Players[playerId].setTryMakeLink(round, targetId, time - self.RewiringTime[round])

    def setPlayerMaintainConnection(self, round, playerId, targetId, time):
        if not self.Players.has_key(playerId): return
        if self.Graphs[round - 1].has_edge(playerId, targetId):
            self.Players[playerId].setNotBreakLink(round, targetId, time - self.RewiringTime[round])
        else:
            self.Players[playerId].setNotMakeLink(round, targetId, time - self.RewiringTime[round])

    def setDropPlayers(self, round, playerId):
        if not self.Drops.has_key(round):
            self.Drops[round] = []
        self.Drops[round].append(playerId)


    def pruneGraph(self):
        import networkx as nx
        isMulti = False
        for r, g in self.Graphs.items():
            gs = nx.connected_component_subgraphs(g)
            if len(gs) > 1:
                isMulti = True
                self.Graphs[r] = gs[0]
            for playerId in self.getPlayersAtRound(r):
                if playerId not in gs[0].nodes():
                    self.Players[playerId].removeStrategy(r)
        if isMulti:
            print""
            print "Warning: this game has isolated small graphs"
            print ""

    def getLinkTargets(self, dict, round):
        if dict.has_key(round):
            return dict[round]
        else:
            return []

    def writeJSON(self, folder = ""):
        import json
        from networkx.readwrite import json_graph
        Anony = {}
        num = 0
        for playerId in self.Players.keys():
            Anony[playerId] = num
            num = num + 1
        DG = {}
        for round in range(self.numRounds + 1): #11
            graph = self.Graphs[round]
            graph.graph["round"] = round
            for n in graph.nodes():
                graph.node[n]["initScore"] = self.Players[n].initialScore
                graph.node[n]["ipAddress"] = self.Players[n].address
                graph.node[n]["gender"] = self.Players[n].Gender
                graph.node[n]["age"] = self.Players[n].Age
                graph.node[n]["is_hispanic"] = self.Players[n].Is_Hispanic
                graph.node[n]["race"] = self.Players[n].Race

                graph.node[n]["interested"] = self.Players[n].interested
                graph.node[n]["distressed"] = self.Players[n].distressed
                graph.node[n]["excited"] = self.Players[n].excited
                graph.node[n]["upset"] = self.Players[n].upset
                graph.node[n]["strong"] = self.Players[n].strong

                graph.node[n]["guilty"] = self.Players[n].guilty
                graph.node[n]["scared"] = self.Players[n].scared
                graph.node[n]["hostile"] = self.Players[n].hostile
                graph.node[n]["enthusiastic"] = self.Players[n].enthusiastic
                graph.node[n]["proud"] = self.Players[n].proud

                graph.node[n]["irritable"] = self.Players[n].irritable
                graph.node[n]["alert"] = self.Players[n].alert
                graph.node[n]["ashamed"] = self.Players[n].ashamed
                graph.node[n]["inspired"] = self.Players[n].inspired
                graph.node[n]["nervous"] = self.Players[n].nervous

                graph.node[n]["determined"] = self.Players[n].determined
                graph.node[n]["attentive"] = self.Players[n].attentive
                graph.node[n]["jittery"] = self.Players[n].jittery
                graph.node[n]["active"] = self.Players[n].active
                graph.node[n]["afraid"] = self.Players[n].afraid

                if round > 0:
                    graph.node[n]["payoff"] = self.Players[n].Payoff[round]
                    graph.node[n]["cumulativePayoff"] = self.Players[n].TotalPayoff[round]
                    if not self.Players[n].Strategy.has_key(round):
                        graph.node[n]["behavior"] =""
                        graph.node[n]["behaviorTime"] = ""
                    else:
                        graph.node[n]["behavior"] = self.Players[n].Strategy[round]
                        graph.node[n]["behaviorTime"] = self.Players[n].StrategyTime[round].seconds * 1000 + (self.Players[n].StrategyTime[round].microseconds)/1000
                    if not self.Players[n].Satisfaction.has_key(round):
                        graph.node[n]["satisfaction"] = ""
                    else:
                        graph.node[n]["satisfaction"] = self.Players[n].Satisfaction[round]
                    graph.node[n]["makeLink"] = self.getLinkTargets(self.Players[n].TryMakeLink, round)
                    graph.node[n]["notMakeLink"] = self.getLinkTargets(self.Players[n].NotMakeLink, round)
                    graph.node[n]["breakLink"] = self.getLinkTargets(self.Players[n].BreakLink, round)
                    graph.node[n]["notBreakLink"] = self.getLinkTargets(self.Players[n].NotBreakLink, round)



            for e in graph.edges():
                graph.edge[e[0]][e[1]]['id'] = e
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
        Game["result"] = DG
        #json.dump(Game,open('../result/json/' + folder + '/' + self.gameId + '.json', 'w'))
        json.dump(Game,open(self.gameId + '.json', 'w'))




